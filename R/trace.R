#' Trace where a number came from
#'
#' `whence_trace()` takes a pipeline that ends in a single value, and runs it
#' one step at a time. It records the number of rows after each step. The
#' result tells you how many rows each step removed, and gives code that
#' makes the value again.
#'
#' The pipeline is split by its verbs. A call to a known verb, such as
#' `filter()` or `summarise()`, is a step, and its first argument is the data
#' that comes from upstream. This works the same for the native pipe `|>`,
#' for the magrittr pipe `%>%` and for nested calls. The chain stops at the
#' first call that is not a known verb, such as a join. That call and all
#' that is upstream of it become the source.
#'
#' Each step is evaluated in `env`, as the full expression would be. A step
#' thus sees the same variables, and in 'shiny' it reads the same inputs and
#' reactive values.
#'
#' The steps run once, in order. This is the same work as running the
#' expression in one piece.
#'
#' # Limits
#'
#' * A database-backed pipeline (a `tbl_lazy` from 'dbplyr') is an error.
#'   Its steps run as SQL only at `collect()`, so there are no row counts to
#'   show.
#' * Joins are not traced. A join ends the chain.
#' * The verbs of 'data.table', 'arrow' and 'duckplyr' are not known.
#' * The native-pipe placeholder `_` is not supported. A pipeline that uses
#'   it is not split, and the value is still given.
#'
#' @param expr A pipeline that ends in a single value: a vector of length
#'   one, or a data frame with one row and one column.
#' @param env The environment in which to evaluate `expr`.
#'
#' @return A `whence_trace` object. Use [whence_value()], [whence_code()],
#'   [whence_rows()], `as.data.frame()` and `print()` with it.
#' @examplesIf requireNamespace("dplyr", quietly = TRUE)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' trace <- whence_trace(
#'   mtcars |>
#'     filter(cyl == 4) |>
#'     filter(mpg > 25) |>
#'     summarise(mean_hp = mean(hp))
#' )
#' trace
#' as.data.frame(trace)
#' @export
whence_trace <- function(expr, env = parent.frame()) {
  quo <- rlang::enquo(expr)
  if (missing(env)) {
    quo_env <- rlang::quo_get_env(quo)
    if (!identical(quo_env, emptyenv())) {
      env <- quo_env
    }
  }
  trace_expr(rlang::quo_get_expr(quo), env)
}


#' Run the count pass on an unevaluated expression
#' @noRd
trace_expr <- function(expr, env) {
  if (rlang::is_quosure(expr)) {
    env <- rlang::quo_get_env(expr)
    expr <- rlang::quo_get_expr(expr)
  }
  while (is.call(expr) && identical(expr[[1]], quote(`{`)) && length(expr) > 1) {
    if (length(expr) > 2) {
      env <- new.env(parent = env)
      for (i in seq.int(2L, length(expr) - 1L)) {
        eval(expr[[i]], env)
      }
    }
    expr <- expr[[length(expr)]]
  }
  parts <- decompose_pipeline(expr)

  current <- eval(parts$source, env)
  check_not_lazy(current)
  source_data <- current

  # The identity pass replays the stages later. slice_sample() draws random
  # numbers, so the replay must start from the same random state.
  seed <- save_seed(parts$stages)

  records <- list(source_record(parts$source, current, env))
  for (i in seq_along(parts$stages)) {
    stage <- parts$stages[[i]]
    stage$call <- substitute_inputs(stage$call, env)
    parts$stages[[i]] <- stage
    before <- current
    current <- eval_stage(stage, current, env)
    check_not_lazy(current)
    records <- c(records, list(stage_record(stage, before, current, env)))
  }

  value <- extract_scalar(current)

  structure(
    list(
      value = value,
      stages = bind_records(records),
      code = build_code(parts$source, parts$stages, env, seed),
      traced = is.data.frame(source_data) && length(parts$stages) > 0,
      expr = expr,
      source = parts$source,
      stage_list = parts$stages,
      source_data = source_data,
      env = env,
      seed = seed
    ),
    class = "whence_trace"
  )
}


#' Evaluate one stage on the upstream data
#'
#' The placeholder is bound in a child of `env`. The stage call thus sees
#' each variable that the full expression would see. A stage that came from
#' `%>%` also gets `.`, as magrittr gives it.
#' @noRd
eval_stage <- function(stage, data, env) {
  stage_env <- new.env(parent = env)
  assign(".whence_data", data, envir = stage_env)
  if (stage$magrittr) {
    assign(".", data, envir = stage_env)
  }
  eval(stage$call, stage_env)
}


# The verbs that aggregate. A drop in rows here is not attrition, so these
# never report a negative delta.
aggregate_verbs <- c("summarise", "count", "tally")

# Arguments of count() that do not name a group.
count_options <- c("wt", "sort", "name", ".drop")


#' The verb of a stage, with `summarize` written as `summarise`
#' @noRd
stage_verb <- function(stage) {
  verb <- stage$verb
  if (is.na(verb)) {
    return(NA_character_)
  }
  if (verb == "summarize") "summarise" else verb
}


#' @noRd
source_record <- function(source, data, env) {
  new_record(
    label = deparse_one(substitute_inputs(source, env)),
    n = row_count(data),
    delta = NA_integer_,
    kind = "source",
    verb = NA_character_,
    groups = if (is_grouped(data)) group_count(data) else NA_integer_,
    shown = TRUE
  )
}


#' @noRd
stage_record <- function(stage, before, after, env) {
  verb <- stage_verb(stage)
  kind <- if (!is.na(verb) && verb %in% whence_verbs) verb else "other"
  n <- row_count(after)
  delta <- n - row_count(before)
  shown <- TRUE

  if (kind %in% aggregate_verbs) {
    delta <- NA_integer_
  }
  if (kind %in% c("group_by", "ungroup")) {
    # A metadata step. It is shown only when it changes the groups.
    delta <- NA_integer_
    shown <- group_count(before) != group_count(after) ||
      !identical(group_names(before), group_names(after))
  }
  if (kind == "pull") {
    shown <- FALSE
  }

  new_record(
    label = stage_label(stage, kind, before, after, env),
    n = n,
    delta = delta,
    kind = kind,
    verb = stage$verb,
    groups = if (is_grouped(after)) group_count(after) else NA_integer_,
    shown = shown
  )
}


#' @noRd
new_record <- function(label, n, delta, kind, verb, groups, shown) {
  data.frame(
    label = label,
    n = as.integer(n),
    delta = as.integer(delta),
    kind = kind,
    verb = verb,
    groups = as.integer(groups),
    shown = shown,
    stringsAsFactors = FALSE
  )
}


#' @noRd
bind_records <- function(records) {
  out <- do.call(rbind, records)
  rownames(out) <- NULL
  out
}


#' The label of a stage in the waterfall
#' @noRd
stage_label <- function(stage, kind, before, after, env) {
  call <- substitute_inputs(stage_display_call(stage), env)
  args <- as.list(call)[-1]

  if (kind == "filter" && !stage$dot && length(args) > 0) {
    return(paste(vapply(args, deparse_one, ""), collapse = ", "))
  }

  if (kind %in% aggregate_verbs) {
    n <- row_count(after)
    noun <- if (identical(n, 1L)) "row" else "rows"
    lead <- if (is_group_aggregate(stage, kind, before)) {
      "grouped into"
    } else {
      "aggregated to"
    }
    return(paste(lead, format_count(n), noun))
  }

  if (kind == "group_by" && !stage$dot && length(args) > 0) {
    return(paste("grouped by", paste(vapply(args, deparse_one, ""), collapse = ", ")))
  }

  if (kind == "ungroup" && length(args) == 0) {
    return("ungrouped")
  }

  deparse_one(call)
}


#' Does an aggregating stage make one row for each group?
#' @noRd
is_group_aggregate <- function(stage, kind, before) {
  if (is_grouped(before)) {
    return(TRUE)
  }
  arg_names <- names(stage$call)[-(1:2)] %||% character(0)
  n_args <- length(stage$call) - 2
  if (n_args == 0) {
    return(FALSE)
  }
  arg_names <- if (length(arg_names) == 0) rep("", n_args) else arg_names

  if (kind == "summarise") {
    return(any(arg_names %in% c(".by", "by")))
  }
  if (kind == "count") {
    return(any(!arg_names %in% count_options))
  }
  FALSE
}


#' Take the single value from the result of a pipeline
#' @noRd
extract_scalar <- function(x) {
  if (!is.data.frame(x) && is.atomic(x) && length(x) == 1) {
    return(x)
  }

  if (is.data.frame(x)) {
    if (nrow(x) == 1 && ncol(x) == 1) {
      return(x[[1]][[1]])
    }
    if (nrow(x) == 1 && ncol(x) > 1) {
      stop(
        "whence can explain a single value. The pipeline returned one row ",
        "with ", ncol(x), " columns: ",
        paste0("`", names(x), "`", collapse = ", "),
        ". Use `select()` or `pull()` to keep one column.",
        call. = FALSE
      )
    }
    stop(
      "whence can explain a single value. The pipeline returned ",
      format_count(nrow(x)), " rows and ", ncol(x), " columns.",
      call. = FALSE
    )
  }

  stop(
    "whence can explain a single value. The pipeline returned ",
    "an object of class `", class(x)[1], "` with length ",
    format_count(length(x)), ".",
    call. = FALSE
  )
}


#' Save the random state, when a stage draws random numbers
#'
#' When no random state exists yet, the first draw would make one. Making it
#' now, with `set.seed(NULL)`, has the same effect, and lets the identity pass
#' replay the same draws.
#' @noRd
save_seed <- function(stages) {
  verbs <- vapply(stages, stage_verb, character(1))
  if (!"slice_sample" %in% verbs) {
    return(NULL)
  }
  if (!exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    set.seed(NULL)
  }
  get(".Random.seed", envir = globalenv(), inherits = FALSE)
}
