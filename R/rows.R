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
  "rename", "group_by", "ungroup", materialising_verbs, join_verbs
)

# The verbs that the identity pass can follow.
tracked_verbs <- c(
  carry_verbs, "select", "transmute", "summarise", "count", "tally", "pull"
)


#' The rows that make the value, or the rows that a step removed
#'
#' `whence_rows()` finds rows of the source data. With no `step`, it gives the
#' rows that contribute to the value: for
#' `sales |> filter(...) |> summarise(total = sum(x))` these are the rows of
#' `sales` that the filters keep. With `step = k`, it gives the rows that step
#' `k` removed. The steps are numbered as in the `step` column of
#' `as.data.frame(trace)`, where the source is step 0.
#'
#' For a group from [whence_group()], both are limited to the rows of that
#' group.
#' After a join, contributing rows are rows of the main source (`x`) only.
#' Repeated matches return each source row once. Rows from `y` are not shown.
#'
#' With `lazy = "count"`, a lazy source has no row attribution until a
#' materialising step such as `collect()` returns a data frame. The identity
#' pass starts from that data frame and follows only later steps. Returned
#' rows are rows of that collected data frame, not rows of the remote source.
#' Requests for earlier steps (or the materialising step itself) give `NULL`
#' with a `whence_rows_unavailable` warning. Without a materialising step,
#' rows are unavailable, even though the final value or table is collected.
#'
#' The function replays the pipeline with a hidden id column on a copy of
#' the source. [whence_trace()] never does this, so the waterfall costs
#' nothing extra. The data that you give is never changed.
#'
#' Some steps destroy the link between a row and the source:
#' `distinct()`, `pivot_longer()`, `pivot_wider()`, and each step that is
#' not a known verb. After such a step the function gives `NULL` with a
#' warning that names the step. The rows that an earlier step removed can
#' still be found. The waterfall of the trace is always complete, because it
#' uses counts only.
#'
#' @param trace A `whence_trace` from [whence_trace()] or [whence_table()],
#'   or a group from [whence_group()].
#' @param n The largest number of rows to give, or `NULL` for all of them.
#' @param step `NULL` for the rows that make the value, or the number of a
#'   step for the rows that it removed.
#'
#' @return Rows of the source data, in their original order, with the
#'   columns of the source. `NULL`, with a warning of class
#'   `whence_rows_unavailable`, when the rows cannot be found.
#' @examplesIf requireNamespace("dplyr", quietly = TRUE)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' trace <- whence_trace(
#'   mtcars |>
#'     filter(cyl == 4) |>
#'     filter(mpg > 30) |>
#'     summarise(mean_hp = mean(hp))
#' )
#' whence_rows(trace)
#'
#' # The cars that the second filter removed
#' whence_rows(trace, step = 2)
#' @export
whence_rows <- function(trace, n = NULL, step = NULL) {
  check_trace(trace)
  if (!is.null(n) && !(is.numeric(n) && length(n) == 1 && !is.na(n) && n >= 0)) {
    stop("`n` must be a single number that is 0 or more, or `NULL`.", call. = FALSE)
  }
  check_step(trace, step)

  result <- rows_query(trace, step)
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


#' Stop unless `step` is NULL or the number of a step of `trace`
#' @noRd
check_step <- function(trace, step) {
  if (is.null(step)) {
    return(invisible())
  }
  last <- nrow(trace$stages) - 1L
  ok <- is.numeric(step) && length(step) == 1 && !is.na(step) && step == round(step) &&
    step >= 1 && step <= last
  if (!ok) {
    if (last < 1) {
      stop("`step` must be `NULL`: this pipeline has no steps.", call. = FALSE)
    }
    stop("`step` must be `NULL` or a whole number from 1 to ", last, ".", call. = FALSE)
  }
  invisible()
}


#' The rows for a trace or a group: all that make the value, or those that
#' one step removed
#'
#' @return A list with `rows`, or `rows = NULL` with `verb` and `reason`.
#' @noRd
rows_query <- function(x, step = NULL) {
  group <- if (inherits(x, "whence_group")) x$row else NULL
  trace <- if (is.null(group)) x else x$table
  origin <- row_origin(trace)
  if (!is.null(origin$reason)) {
    return(lost_identity(NA_character_, origin$reason))
  }
  if (!is.null(step) && step <= origin$step) {
    return(lost_identity(NA_character_, origin$prefix_reason))
  }
  replay <- x$replay %||% replay_trace(trace)
  sources <- replay$sources
  codes <- replay$codes

  in_group <- function(s) {
    if (is.null(group)) {
      return(unlist(sources[[s]]))
    }
    if (is.null(codes[[s]])) {
      return(NULL)
    }
    unlist(sources[[s]][codes[[s]] %in% group])
  }

  if (is.null(step)) {
    if (!replay$ok) {
      return(replay[c("rows", "verb", "reason")])
    }
    return(list(rows = source_rows(trace, in_group(length(sources)))))
  }

  if (length(sources) < step + 1L) {
    return(replay[c("rows", "verb", "reason")])
  }
  before <- in_group(step)
  after <- in_group(step + 1L)
  if (!is.null(group) && (is.null(before) || is.null(after))) {
    return(list(
      rows = NULL,
      verb = NA_character_,
      reason = "Row attribution unavailable: the group columns do not exist at this step."
    ))
  }
  list(rows = source_rows(trace, setdiff(before, after)))
}


#' The source rows that make the value, or why they cannot be found
#'
#' @return A list with `rows` (the source rows, or `NULL`), and when `rows`
#'   is `NULL`, `verb` and `reason`.
#' @noRd
identity_pass <- function(trace) {
  replay <- replay_trace(trace)
  if (!replay$ok) {
    return(replay[c("rows", "verb", "reason")])
  }
  last <- replay$sources[[length(replay$sources)]]
  list(rows = source_rows(trace, unlist(last)))
}


#' Replay the stages with the hidden id column, and record each step
#'
#' After each step it records, for each row of the data, the source rows that
#' it stands for (`sources`: an integer vector before a summary, a list after
#' one), and, for a table, the row of the table that each row belongs to
#' (`codes`). Element 1 is the source, element k + 1 is after step k. When the
#' replay cannot go on, `ok` is `FALSE` and the steps before that point are
#' still recorded.
#' @noRd
replay_trace <- function(trace) {
  out <- list(ok = FALSE, rows = NULL, verb = NA_character_, reason = NULL, sources = list(), codes = list())
  # The record grows in an environment, so that record() can add to it.
  rec <- new.env(parent = emptyenv())
  rec$sources <- list()
  rec$codes <- list()
  stop_with <- function(lost) {
    out[c("verb", "reason")] <- lost[c("verb", "reason")]
    out$sources <- rec$sources
    out$codes <- rec$codes
    out
  }
  if (!trace$traced) {
    return(stop_with(lost_identity(NA_character_, "the pipeline was not traced")))
  }
  origin <- row_origin(trace)
  if (!is.null(origin$reason)) {
    return(stop_with(lost_identity(NA_character_, origin$reason)))
  }
  source <- origin$data
  # Empty slots keep the replay aligned with the original step numbers.
  rec$sources <- vector("list", origin$step)
  rec$codes <- vector("list", origin$step)
  if (id_col %in% names(source)) {
    return(stop_with(lost_identity(NA_character_, "the source has a column named .whence_id")))
  }

  if (!is.null(trace$seed)) {
    old_seed <- swap_seed(trace$seed)
    on.exit(reset_seed(old_seed), add = TRUE)
  }

  keys <- trace$keys
  record <- function(data, id_map, ids = NULL) {
    row_ids <- ids %||% data[[id_col]]
    index <- as.integer(substring(row_ids, 2))
    sources <- if (is.null(id_map)) index else unname(id_map[index])
    codes <- if (!is.null(keys) && is.data.frame(data) && all(keys %in% names(data))) {
      key_codes(data, trace$value, keys)
    }
    rec$sources[[length(rec$sources) + 1L]] <- sources
    rec$codes[length(rec$codes) + 1L] <- list(codes)
  }

  data <- source
  data[[id_col]] <- paste0("w", seq_len(nrow(source)), recycle0 = TRUE)
  id_map <- NULL
  record(data, id_map)
  stages <- trace$stage_list

  for (i in seq_along(stages)) {
    if (i <= origin$step) {
      next
    }
    stage <- stages[[i]]
    verb <- stage_verb(stage)
    if (is.na(verb) || !verb %in% tracked_verbs) {
      return(stop_with(lost_identity(verb)))
    }
    if (verb == "pull" && i != length(stages)) {
      return(stop_with(lost_identity(verb)))
    }

    step <- tryCatch(
      replay_stage(stage, verb, data, trace$env, id_map),
      error = function(e) NULL
    )
    if (is.null(step)) {
      return(stop_with(lost_identity(verb)))
    }
    if (!is.null(step$reason)) {
      return(stop_with(step))
    }
    expected_n <- trace$stages$n[[i + 1L]]
    if (!is.na(expected_n) && !identical(row_count(step$data), expected_n)) {
      return(stop_with(lost_identity(verb, paste0("the hidden id changed the result of ", verb, "()"))))
    }
    data <- step$data
    id_map <- step$id_map
    record(data, id_map, step$ids)
  }

  out$ok <- TRUE
  out$sources <- rec$sources
  out$codes <- rec$codes
  out
}


#' The source rows with these row numbers, in their original order
#' @noRd
source_rows <- function(trace, ids) {
  row_origin(trace)$data[sort(unique(as.integer(ids))), , drop = FALSE]
}


# Use local rows only. Remote steps cannot carry a stable hidden id.
row_origin <- function(trace) {
  if (!is_lazy_table(trace$source_data)) {
    return(list(data = trace$source_data, step = 0L))
  }
  origin <- trace$row_source
  if (is.null(origin)) {
    return(list(reason = "the data is a lazy table"))
  }
  origin$prefix_reason <- paste0(
    "steps before ", origin$verb, "() run ", origin$location
  )
  origin
}


#' The row of `table` that each row of `data` belongs to, by the key columns
#'
#' Each key is matched as integer codes, so NA and "NA" stay apart.
#' @noRd
key_codes <- function(data, table, keys) {
  codes <- lapply(keys, function(key) {
    values <- c(as.character(table[[key]]), as.character(data[[key]]))
    match(values, unique(values))
  })
  combined <- do.call(paste, c(codes, sep = ":"))
  table_keys <- combined[seq_len(nrow(table))]
  data_keys <- combined[nrow(table) + seq_len(nrow(data))]
  match(data_keys, table_keys)
}


#' Replay one stage, and check that the id column survived it
#' @noRd
replay_stage <- function(stage, verb, data, env, id_map) {
  before_ids <- data[[id_col]]

  if (verb %in% join_verbs) {
    stage$call <- join_call(stage)
    y <- eval_stage(
      list(call = stage$call$y, magrittr = stage$magrittr), data, env
    )
    if (id_col %in% names(y)) {
      return(lost_identity(verb, paste0(
        "`y` in ", verb, "() has a column named `.whence_id`"
      )))
    }
    # Bind y once so a check for the reserved name does not run it twice.
    env <- new.env(parent = env)
    env$.whence_join_y <- y
    stage$call$y <- quote(.whence_join_y)
  }

  if (verb == "pull") {
    eval_stage(stage, data, env)
    return(list(data = NULL, id_map = id_map, ids = before_ids))
  }

  if (verb %in% c("summarise", "count", "tally")) {
    return(replay_aggregate(stage, verb, data, env, id_map))
  }

  stage$call <- keep_id_call(stage$call, verb)
  out <- if (verb %in% join_verbs) {
    suppressWarnings(suppressMessages(eval_stage(stage, data, env)))
  } else {
    eval_stage(stage, data, env)
  }
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
  if (inherits(trace, "whence_group")) {
    if (!trace$replay$ok) {
      return(trace$replay$reason)
    }
    trace <- trace$table
  }
  origin <- row_origin(trace)
  if (!is.null(origin$reason)) {
    return(lost_identity(NA_character_, origin$reason)$reason)
  }
  if (!trace$traced) {
    return(lost_identity(NA_character_, "the pipeline was not traced")$reason)
  }
  if (id_col %in% names(origin$data)) {
    return(lost_identity(NA_character_, "the source has a column named .whence_id")$reason)
  }
  verbs <- vapply(trace$stage_list, stage_verb, character(1))
  for (i in seq_along(verbs)) {
    if (i <= origin$step) {
      next
    }
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
