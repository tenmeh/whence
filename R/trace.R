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
#' first call that is not a known verb, such as `full_join()`. That call and all
#' that is upstream of it become the source.
#'
#' Each step is evaluated in `env`, as the full expression would be. A step
#' thus sees the same variables, and in 'shiny' it reads the same inputs and
#' reactive values.
#'
#' Step calls are evaluated once, in order. An `inner_join()` also runs an
#' `anti_join()` to count unmatched rows. Repeated join matches are counted
#' separately from removed rows in the integer `added` column (`NA` for other
#' verbs).
#'
#' # Limits
#'
#' * Lazy tables from 'dbplyr', 'arrow', 'dtplyr', and prudent 'duckplyr'
#'   need `lazy = "count"` to count remote steps. With `lazy = "stop"`, the
#'   last materialising step becomes the source and only later steps are
#'   counted. A table that is still lazy gives an error in this mode.
#'   Default 'duckplyr' data frames work without opting in.
#' * `right_join()` and `full_join()` end the chain. They can bring in rows
#'   that are not from the main source.
#' * Native 'data.table' syntax is not traced.
#' * The native-pipe placeholder `_` is not supported. A pipeline that uses
#'   it is not split, and the value is still given.
#'
#' # Known verbs
#'
#' These calls are steps: `filter()`, `mutate()`, `transmute()`, `select()`,
#' `rename()`, `arrange()`, `slice()`, `slice_head()`, `slice_tail()`,
#' `slice_min()`, `slice_max()`, `slice_sample()`, `group_by()`, `ungroup()`,
#' `summarise()`, `summarize()`, `count()`, `tally()`, `distinct()`,
#' `drop_na()`, `pivot_longer()`, `pivot_wider()`, `pull()`, `head()` and
#' `tail()`. Joins `inner_join()`, `left_join()`, `semi_join()` and
#' `anti_join()` are steps too. Materialising verbs `collect()`, `compute()`,
#' `as_tibble()` and `as.data.frame()` keep rows and are hidden in the
#' waterfall. All work with or without a namespace such as `dplyr::`.
#' Any other call ends the chain, and the trace says where in its `reason`.
#'
#' # Lazy tables
#'
#' Use `lazy = "count"` to trace a lazy pipeline without collecting every
#' intermediate table. This runs one count query for the source and one for
#' each lazy step, plus one for each `inner_join()` to count unmatched rows.
#' Local steps need no count query. The final result is collected to give
#' the value or table. The reproduce code includes this final `collect()`.
#' Count queries and the final collection can be costly on large tables.
#' Each count can run the upstream query again.
#'
#' Set `options(whence.lazy = "count")` to use this mode by default, including
#' in the render functions. `lazy = "stop"` keeps the default behaviour.
#' Row attribution starts only after a materialising step in the pipeline;
#' it cannot identify rows of a remote source. See [whence_rows()].
#'
#' @param expr A pipeline that ends in a single value: a vector of length
#'   one, or a data frame with one row and one column.
#' @param env The environment in which to evaluate `expr`.
#' @param lazy How to handle lazy tables: `"stop"` or `"count"`, with no
#'   partial matching. Defaults to `getOption("whence.lazy", "stop")`.
#'
#' @return A `whence_trace` object. Use [whence_value()], [whence_code()],
#'   [whence_rows()], `as.data.frame()` and `print()` with it.
#'
#'   The object is a list. These fields are stable, and you can read them:
#'
#'   * `value`: the value, as [whence_value()] gives it.
#'   * `code`: the reproduce code, as [whence_code()] gives it.
#'   * `traced`: `TRUE` when the pipeline was split into steps.
#'   * `stages`: a data frame with one row for each step, as
#'     `as.data.frame()` gives it.
#'   * `reason`: why the chain stopped short, as one sentence, or `NULL` when
#'     every step was traced.
#'
#'   Do not rely on the other fields. They can change in any version.
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
whence_trace <- function(expr, env = parent.frame(), lazy = getOption("whence.lazy", "stop")) {
  check_lazy(lazy)
  quo <- rlang::enquo(expr)
  if (missing(env)) {
    quo_env <- rlang::quo_get_env(quo)
    if (!identical(quo_env, emptyenv())) {
      env <- quo_env
    }
  }
  trace_expr(rlang::quo_get_expr(quo), env, lazy = lazy)
}


#' Run the count pass on an unevaluated expression
#' @noRd
trace_expr <- function(expr, env, mode = "value", lazy = getOption("whence.lazy", "stop")) {
  check_lazy(lazy)
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
  materialised_at <- NULL
  if (is_lazy_table(current) && lazy == "stop") {
    lazy_location <- lazy_table_kind(current)$location
    verbs <- vapply(parts$stages, stage_verb, "")
    materialisers <- which(verbs %in% materialising_verbs)
    if (length(materialisers) == 0) {
      stop_lazy_table(current)
    }
    last <- max(materialisers)
    materialised_at <- stage_verb(parts$stages[[last]])
    # Keep the lazy prefix as the source, without evaluating the source twice.
    for (i in seq_len(last)) {
      stage <- parts$stages[[i]]
      current <- eval_stage(stage, current, env)
      if (stage$magrittr && "." %in% all.names(stage$call)) {
        body <- replace_symbol(stage$call, placeholder_sym, quote(.))
        fn <- as.call(list(quote(`function`), as.pairlist(alist(. = )), body))
        parts$source <- as.call(list(fn, parts$source))
      } else {
        parts$source <- replace_symbol(stage$call, placeholder_sym, parts$source)
      }
    }
    parts$stages <- parts$stages[-seq_len(last)]
  }
  if (is_lazy_table(current) && lazy == "stop") {
    stop_lazy_table(current)
  }
  source_data <- current
  lazy_source <- is_lazy_table(source_data)
  row_source <- NULL

  # The identity pass replays the stages later. slice_sample() draws random
  # numbers, so the replay must start from the same random state.
  seed <- save_seed(parts$stages)

  current_n <- row_count(current, lazy)
  records <- list(source_record(parts$source, current, env, n = current_n))
  keys <- NULL
  for (i in seq_along(parts$stages)) {
    stage <- parts$stages[[i]]
    stage$call <- substitute_inputs(stage$call, env)
    parts$stages[[i]] <- stage
    before <- current
    before_n <- current_n
    current <- eval_stage(stage, current, env)
    if (is_lazy_table(current) && lazy == "stop") {
      stop_lazy_table(current)
    }
    # Reuse the upstream count. Labels and percentages must not issue queries.
    current_n <- row_count(current, lazy)
    records <- c(records, list(stage_record(
      stage, before, current, env, before_n = before_n, n = current_n, lazy = lazy
    )))
    # Rows can be found only from the first step that returns local data.
    first_local <- lazy_source && is.null(row_source) &&
      stage_verb(stage) %in% materialising_verbs && is.data.frame(current) && !is_lazy_table(current)
    if (first_local) {
      row_source <- list(
        data = current, step = i, verb = stage_verb(stage),
        location = lazy_table_kind(source_data)$location
      )
    }
    if (stage_verb(stage) %in% aggregate_verbs) {
      # The groups of the last summary are the rows of a table.
      keys <- aggregate_keys(stage, stage_verb(stage), before, current)
    }
  }

  collected_result <- is_lazy_table(current)
  if (collected_result) {
    current <- dplyr::collect(current)
  }
  value <- if (identical(mode, "table")) extract_table(current) else extract_scalar(current)
  code <- build_code(parts$source, parts$stages, env, seed)
  if (collected_result) {
    code <- paste0(code, " |>\n  dplyr::collect()")
  }

  structure(
    list(
      value = value,
      stages = bind_records(records),
      keys = if (identical(mode, "table")) table_keys(keys, value) else NULL,
      code = code,
      traced = (is.data.frame(source_data) || lazy_source) && length(parts$stages) > 0,
      reason = if (!is.null(materialised_at)) paste0(
        "Steps before ", materialised_at,
        "() run ", lazy_location, ", so they are not counted."
      ) else trace_reason(parts$source, length(parts$stages), source_data),
      expr = expr,
      source = parts$source,
      stage_list = parts$stages,
      source_data = source_data,
      row_source = row_source,
      env = env,
      seed = seed
    ),
    class = if (identical(mode, "table")) c("whence_table", "whence_trace") else "whence_trace"
  )
}


#' The columns that name the groups of a summary step
#'
#' A grouped input gives its grouping columns. `count()` puts its count in the
#' last column, so the other columns are the groups. `summarise(.by = )`
#' keeps the `.by` columns, which are the input columns that the summary did
#' not name.
#' @noRd
aggregate_keys <- function(stage, verb, before, after) {
  if (verb == "count") {
    return(utils::head(column_names(after), -1))
  }
  if (is_grouped(before)) {
    return(group_names(before))
  }
  if (verb == "tally") {
    return(character(0))
  }
  arg_names <- names(stage$call)[-(1:2)] %||% character(0)
  summary_names <- arg_names[nzchar(arg_names) & !arg_names %in% c(".by", "by", ".groups")]
  setdiff(intersect(column_names(after), column_names(before)), summary_names)
}


#' The group columns of a table, when every one of them is in the table
#' @noRd
table_keys <- function(keys, value) {
  if (length(keys) == 0 || !all(keys %in% column_names(value))) {
    return(NULL)
  }
  keys
}


#' Check the result of a table pipeline
#' @noRd
extract_table <- function(x) {
  if (!is.data.frame(x)) {
    stop(
      "whence can explain a table. The pipeline returned an object of class `",
      class(x)[1], "`, not a data frame.",
      call. = FALSE
    )
  }
  x
}


#' Why the chain stopped short, as one sentence, or `NULL`
#' @noRd
trace_reason <- function(source, n_stages, source_data) {
  stop_at <- stop_point(source)
  if (n_stages > 0 && (is.data.frame(source_data) || is_lazy_table(source_data))) {
    if (is.null(stop_at)) {
      return(NULL)
    }
    return(paste0(
      "Steps before ", stop_at, "() are not shown, because whence does not know ",
      stop_at, "()."
    ))
  }
  if (!is.null(stop_at)) {
    return(paste0("Not traced: the chain stops at ", stop_at, "(), which whence does not know."))
  }
  if (n_stages > 0) {
    return("Not traced: the source is not a data frame.")
  }
  "Not traced: the pipeline has no step that whence knows."
}


#' The name of the call where the chain stopped, or `NULL`
#'
#' The chain stopped at a call that is not a known verb. When that call has
#' a pipeline of known verbs as its first argument, the user wrote more steps
#' than whence could follow, so the call is named.
#' @noRd
stop_point <- function(source) {
  if (!is.call(source) || length(source) < 2 || is_magrittr_pipe(source)) {
    return(NULL)
  }
  name <- safe_call_name(source)
  if (is.null(name) || name %in% whence_verbs) {
    return(NULL)
  }
  first <- source[[2]]
  if (is_verb_call(first) || is_magrittr_pipe(first)) name else NULL
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


# Match positional join options before dropping options for mutating joins.
join_call <- function(stage) {
  match.call(
    function(x, y, by = NULL, copy = FALSE, suffix = NULL, ..., keep = NULL) NULL,
    stage$call,
    expand.dots = TRUE
  )
}


anti_join_stage <- function(stage) {
  call <- join_call(stage)
  fn <- call[[1]]
  if (is.call(fn)) {
    fn[[3]] <- as.symbol("anti_join")
  } else {
    fn <- as.symbol("anti_join")
  }
  args <- as.list(call)[-1]
  args <- args[names(args) %in% c("x", "y", "by", "copy", "na_matches")]
  stage$call <- as.call(c(list(fn), args))
  stage
}

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
source_record <- function(source, data, env, n = row_count(data)) {
  new_record(
    label = deparse_one(substitute_inputs(source, env)),
    n = n,
    delta = NA_integer_,
    kind = "source",
    verb = NA_character_,
    groups = if (is_grouped(data)) group_count(data) else NA_integer_,
    shown = TRUE,
    percent = NA_real_
  )
}


#' @noRd
stage_record <- function(stage, before, after, env, before_n = row_count(before, lazy),
                         n = row_count(after, lazy), lazy = "stop") {
  verb <- stage_verb(stage)
  kind <- if (!is.na(verb) && verb %in% whence_verbs) verb else "other"
  delta <- n - before_n
  added <- NA_integer_
  shown <- TRUE

  if (kind %in% join_verbs) {
    added <- 0L
    if (kind == "left_join") {
      delta <- 0L
      added <- n - before_n
    } else if (kind == "inner_join") {
      unmatched <- suppressWarnings(suppressMessages(
        eval_stage(anti_join_stage(stage), before, env)
      ))
      delta <- -suppressWarnings(suppressMessages(row_count(unmatched, lazy)))
      added <- n - (before_n + delta)
    }
  }

  if (kind %in% aggregate_verbs) {
    delta <- NA_integer_
  }
  if (kind %in% c("group_by", "ungroup")) {
    # A metadata step. It is shown only when it changes the groups.
    delta <- NA_integer_
    shown <- !identical(group_count(before), group_count(after)) ||
      !identical(group_names(before), group_names(after))
  }
  if (kind %in% c("pull", materialising_verbs)) {
    delta <- NA_integer_
    shown <- FALSE
  }
  percent <- if (is.na(delta) || delta == 0 || is.na(before_n) || before_n == 0) {
    NA_real_
  } else {
    100 * delta / before_n
  }

  new_record(
    label = stage_label(stage, kind, before, after, env, n = n),
    n = n,
    delta = delta,
    kind = kind,
    verb = stage$verb,
    groups = if (is_grouped(after)) group_count(after) else NA_integer_,
    shown = shown,
    percent = percent,
    added = added
  )
}


#' @noRd
new_record <- function(label, n, delta, kind, verb, groups, shown, percent,
                       added = NA_integer_) {
  data.frame(
    label = label,
    n = as.integer(n),
    delta = as.integer(delta),
    percent = as.numeric(percent),
    kind = kind,
    verb = verb,
    groups = as.integer(groups),
    shown = shown,
    added = as.integer(added),
    stringsAsFactors = FALSE
  )
}


#' Bind the records, and number the steps: 0 is the source
#' @noRd
bind_records <- function(records) {
  out <- do.call(rbind, records)
  rownames(out) <- NULL
  cbind(step = seq_len(nrow(out)) - 1L, out)
}


#' A percentage as text: "50%", "<1%" for a small one, "" for none
#'
#' A share that rounds to 100% but is not all of the rows is "99%", so that
#' "100%" always means every row.
#' @noRd
format_percent <- function(percent) {
  size <- abs(percent)
  rounded <- ifelse(size < 100 & round(size) >= 100, 99, round(size))
  out <- ifelse(is.na(percent), "", ifelse(size < 1, "<1%", paste0(rounded, "%")))
  unname(out)
}


#' The label of a stage in the waterfall
#' @noRd
stage_label <- function(stage, kind, before, after, env, n = row_count(after)) {
  call <- substitute_inputs(stage_display_call(stage), env)
  args <- as.list(call)[-1]

  if (kind == "filter" && !stage$dot && length(args) > 0) {
    return(paste(vapply(args, deparse_one, ""), collapse = ", "))
  }

  if (kind %in% aggregate_verbs) {
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
      format_count(nrow(x)), " rows and ", ncol(x), " columns. ",
      "For a table, use `whence_table()`, or `renderWhenceTable()` in 'shiny'.",
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
