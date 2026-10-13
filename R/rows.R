# The identity pass.
#
# The waterfall needs counts only. To know WHICH source rows make the value,
# the stages are replayed on a copy of the source with a hidden id column.
# This changes what some verbs do, so it runs only when it is asked for, and
# it is allowed to fail.
#
# The id column is character ("w1", "w2", ...). A numeric id could be
# changed with no error by a step such as `mutate(across(everything(), ...))`
# and give a wrong answer. Arithmetic on a character column stops with an
# error instead, and each id is checked after each step.


# The name of the hidden id column. It never leaves this file.
id_col <- ".whence_id"

# The verbs that keep the id column with no change to the call.
carry_verbs <- c(
  "filter", "arrange", "slice", "slice_head", "slice_tail", "slice_min",
  "slice_max", "slice_sample", "head", "tail", "drop_na", "mutate",
  "rename", "group_by", "ungroup"
)

# The verbs that the identity pass can follow.
tracked_verbs <- c(
  carry_verbs, "select", "transmute", "summarise", "count", "tally", "pull"
)


#' The rows that make the value
#'
#' `whence_rows()` finds the rows of the source data that contribute to the
#' value of a trace. For `sales |> filter(...) |> summarise(total = sum(x))`
#' these are the rows of `sales` that the filters keep.
#'
#' The function replays the pipeline with a hidden id column on a copy of
#' the source. [whence_trace()] never does this, so the waterfall costs
#' nothing extra. The data that you give is never changed.
#'
#' Some steps destroy the link between a row and the source:
#' `distinct()`, `pivot_longer()`, `pivot_wider()`, and each step that is
#' not a known verb. After such a step the function gives `NULL` with a
#' warning that names the step. The waterfall of the trace is still
#' complete, because it uses counts only.
#'
#' @param trace A `whence_trace` from [whence_trace()].
#' @param n The largest number of rows to give, or `NULL` for all of them.
#'
#' @return The contributing rows of the source data, in their original
#'   order, with the columns of the source. `NULL`, with a warning of class
#'   `whence_rows_unavailable`, when the rows cannot be found.
#' @examplesIf requireNamespace("dplyr", quietly = TRUE)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' trace <- whence_trace(
#'   mtcars |>
#'     filter(cyl == 4, mpg > 30) |>
#'     summarise(mean_hp = mean(hp))
#' )
#' whence_rows(trace)
#' @export
whence_rows <- function(trace, n = NULL) {
  check_trace(trace)
  if (!is.null(n) && !(is.numeric(n) && length(n) == 1 && !is.na(n) && n >= 0)) {
    stop("`n` must be a single number that is 0 or more, or `NULL`.", call. = FALSE)
  }

  result <- identity_pass(trace)
  if (is.null(result$rows)) {
    warning(
      structure(
        class = c("whence_rows_unavailable", "warning", "condition"),
        list(message = result$reason, call = NULL, verb = result$verb)
      )
    )
    return(NULL)
  }

  rows <- result$rows
  if (!is.null(n) && nrow(rows) > n) {
    rows <- rows[seq_len(n), , drop = FALSE]
  }
  rows
}


#' Replay the stages with the hidden id column
#'
#' @return A list with `rows` (the source rows, or `NULL`), and when `rows`
#'   is `NULL`, `verb` and `reason`.
#' @noRd
identity_pass <- function(trace) {
  if (!trace$traced) {
    return(lost_identity(NA_character_, "the pipeline was not traced"))
  }
  source <- trace$source_data
  if (id_col %in% names(source)) {
    return(lost_identity(NA_character_, "the source has a column named .whence_id"))
  }

  if (!is.null(trace$seed)) {
    old_seed <- swap_seed(trace$seed)
    on.exit(reset_seed(old_seed), add = TRUE)
  }

  data <- source
  data[[id_col]] <- paste0("w", seq_len(nrow(source)), recycle0 = TRUE)
  id_map <- NULL
  ids <- NULL
  stages <- trace$stage_list

  for (i in seq_along(stages)) {
    stage <- stages[[i]]
    verb <- stage_verb(stage)
    if (is.na(verb) || !verb %in% tracked_verbs) {
      return(lost_identity(verb))
    }
    if (verb == "pull" && i != length(stages)) {
      return(lost_identity(verb))
    }

    step <- tryCatch(
      replay_stage(stage, verb, data, trace$env, id_map),
      error = function(e) NULL
    )
    if (is.null(step)) {
      return(lost_identity(verb))
    }
    expected_n <- trace$stages$n[[i + 1L]]
    if (!is.na(expected_n) && !identical(row_count(step$data), expected_n)) {
      return(lost_identity(verb, paste0("the hidden id changed the result of ", verb, "()")))
    }
    data <- step$data
    id_map <- step$id_map
    ids <- step$ids
  }

  if (is.null(ids)) {
    ids <- data[[id_col]]
  }
  source_ids <- resolve_ids(ids, id_map)
  list(rows = source[sort(unique(source_ids)), , drop = FALSE])
}


#' Replay one stage, and check that the id column survived it
#' @noRd
replay_stage <- function(stage, verb, data, env, id_map) {
  before_ids <- data[[id_col]]

  if (verb == "pull") {
    eval_stage(stage, data, env)
    return(list(data = NULL, id_map = id_map, ids = before_ids))
  }

  if (verb %in% c("summarise", "count", "tally")) {
    return(replay_aggregate(stage, verb, data, env, id_map))
  }

  stage$call <- keep_id_call(stage$call, verb)
  out <- eval_stage(stage, data, env)
  check_ids(out, before_ids)
  list(data = out, id_map = id_map, ids = NULL)
}


#' Rewrite a call so that it keeps the id column
#'
#' `select()` and `transmute()` drop each column that they do not name.
#' `mutate()` drops columns too when it has a `.keep` argument.
#' @noRd
keep_id_call <- function(call, verb) {
  arg_names <- names(call) %||% rep("", length(call))
  if (verb == "select") {
    call[[length(call) + 1]] <- as.symbol(id_col)
  }
  if (verb == "transmute" || (verb == "mutate" && ".keep" %in% arg_names)) {
    call[[id_col]] <- as.symbol(id_col)
  }
  call
}


#' Stop unless the id column is intact
#' @noRd
check_ids <- function(out, before_ids) {
  ids <- if (is.data.frame(out)) out[[id_col]] else NULL
  if (!is.character(ids) || anyNA(match(ids, before_ids))) {
    stop("The id column did not survive the step.", call. = FALSE)
  }
  invisible(out)
}


#' Replay an aggregating stage, and record which ids make each output row
#' @noRd
replay_aggregate <- function(stage, verb, data, env, id_map) {
  if (verb == "summarise") {
    # Each group gives the list of its own ids, next to the user's results.
    stage$call[[".whence_ids"]] <- call("list", as.symbol(id_col))
    out <- eval_stage(stage, data, env)
    groups <- out[[".whence_ids"]]
    out[[".whence_ids"]] <- NULL
  } else {
    out <- eval_stage(stage, data, env)
    groups <- ids_by_key(data, out)
  }

  if (!is.list(groups) || length(groups) != nrow(out)) {
    stop("The groups could not be matched to the result.", call. = FALSE)
  }

  new_map <- lapply(groups, resolve_ids, id_map = id_map)
  out[[id_col]] <- paste0("w", seq_len(nrow(out)), recycle0 = TRUE)
  list(data = out, id_map = new_map, ids = NULL)
}


#' Match the rows of a count() or tally() result to the input rows
#'
#' The result holds the grouping columns and one new count column. Each
#' input row goes to the output row with the same values in those columns.
#' @noRd
ids_by_key <- function(data, out) {
  keys <- intersect(names(out), setdiff(names(data), id_col))
  if (length(keys) != ncol(out) - 1) {
    # A group that was computed in the call is not in the input.
    stop("A grouping column is not in the input.", call. = FALSE)
  }
  ids <- data[[id_col]]
  if (length(keys) == 0) {
    return(list(ids))
  }
  codes <- lapply(keys, function(key) {
    values <- c(data[[key]], out[[key]])
    match(values, unique(values))
  })
  combined <- do.call(paste, c(codes, sep = ":"))
  input_keys <- combined[seq_len(nrow(data))]
  output_keys <- combined[nrow(data) + seq_len(nrow(out))]
  index <- match(input_keys, output_keys)
  unname(split(ids, factor(index, levels = seq_len(nrow(out)))))
}


#' Turn ids of the current data into row numbers of the source
#' @noRd
resolve_ids <- function(ids, id_map) {
  index <- as.integer(substring(ids, 2))
  if (is.null(id_map)) {
    return(index)
  }
  as.integer(unlist(id_map[index]))
}


#' The result of an identity pass that cannot continue
#' @noRd
lost_identity <- function(verb, why = NULL) {
  reason <- if (!is.null(why)) {
    paste0("Row attribution unavailable: ", why, ".")
  } else if (is.na(verb)) {
    "Row attribution unavailable past a step that is not a known verb."
  } else {
    paste0("Row attribution unavailable past ", verb, "().")
  }
  list(rows = NULL, verb = verb, reason = reason)
}


#' Why the identity pass cannot follow the stages, or `NULL` when it can
#'
#' This looks at the stages only. It evaluates nothing.
#' @noRd
rows_unavailable_reason <- function(trace) {
  if (!trace$traced) {
    return(lost_identity(NA_character_, "the pipeline was not traced")$reason)
  }
  if (id_col %in% names(trace$source_data)) {
    return(lost_identity(NA_character_, "the source has a column named .whence_id")$reason)
  }
  verbs <- vapply(trace$stage_list, stage_verb, character(1))
  for (i in seq_along(verbs)) {
    verb <- verbs[[i]]
    if (is.na(verb) || !verb %in% tracked_verbs || (verb == "pull" && i != length(verbs))) {
      return(lost_identity(verb)$reason)
    }
  }
  NULL
}


#' Put in the random state of the count pass, for a replay
#'
#' @return The random state before the swap, or `NULL` when there was none.
#' @noRd
swap_seed <- function(seed) {
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  assign(".Random.seed", seed, envir = globalenv()) # nolint: object_name_linter.
  old
}


#' Put back the random state of the caller
#' @noRd
reset_seed <- function(old) {
  if (is.null(old)) {
    rm(".Random.seed", envir = globalenv())
  } else {
    assign(".Random.seed", old, envir = globalenv()) # nolint: object_name_linter.
  }
}
