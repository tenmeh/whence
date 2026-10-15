# Tables: a pipeline that ends in a summary with one row for each group.
#
# The table is traced as a whole, in one count pass, as for a value. The
# explanation of one row is made only when it is asked for. It does not run
# the pipeline again for the group: a step after the summary that compares
# groups, such as mutate(share = total / sum(total)), would then give a
# wrong value. It replays the steps once with the hidden id column, and reads
# the rows of the group at each step from the record of that replay.


#' Trace where the rows of a summary table came from
#'
#' `whence_table()` is [whence_trace()] for a pipeline that ends in a table,
#' such as `group_by(region) |> summarise(total = sum(revenue))`. The trace
#' covers the table as a whole. [whence_group()] then explains one row of
#' it: how many rows of its group each step removed, the code that makes
#' that row, and the rows behind it.
#'
#' The rows of the table are the groups of its last summary step
#' (`summarise()`, `count()` or `tally()`). A row can be explained when every
#' group column of that step is still in the table.
#'
#' @inheritParams whence_trace
#' @param expr A pipeline that ends in a data frame.
#'
#' @return A `whence_table` object, which is also a `whence_trace`. Its value
#'   is the table. Its field `keys` names the group columns, or is `NULL` when
#'   the rows cannot be explained one by one.
#' @examplesIf requireNamespace("dplyr", quietly = TRUE)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' by_cyl <- whence_table(
#'   mtcars |>
#'     filter(hp > 90) |>
#'     group_by(cyl) |>
#'     summarise(mpg = mean(mpg))
#' )
#' by_cyl
#' whence_group(by_cyl, 2)
#' @export
whence_table <- function(expr, env = parent.frame()) {
  quo <- rlang::enquo(expr)
  if (missing(env)) {
    quo_env <- rlang::quo_get_env(quo)
    if (!identical(quo_env, emptyenv())) {
      env <- quo_env
    }
  }
  trace_expr(rlang::quo_get_expr(quo), env, mode = "table")
}


#' Explain one row of a summary table
#'
#' `whence_group()` takes a table from [whence_table()] and the number of a
#' row. It gives a trace of that row: for each step, how many rows of its
#' group were left, and how many the step removed. Use [whence_rows()] on it
#' for the rows of the group, or for the rows of the group that one step
#' removed.
#'
#' The counts come from a replay of the steps with a hidden id column. After
#' a step that breaks the link between a row and the source, such as
#' `distinct()`, the counts of the group are not known, and the trace says
#' so. The reproduce code is the code of the table with a last `filter()`
#' that keeps this row, so it gives the row as the table has it.
#'
#' @param table A `whence_table` from [whence_table()].
#' @param row The number of a row of the table.
#'
#' @return A `whence_group` object, which is also a `whence_trace`. Its value
#'   is the row, as a data frame with one row.
#' @examplesIf requireNamespace("dplyr", quietly = TRUE)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' by_cyl <- whence_table(
#'   mtcars |>
#'     filter(hp > 90) |>
#'     count(cyl)
#' )
#' six <- whence_group(by_cyl, 2)
#' six
#' whence_rows(six)
#' @export
whence_group <- function(table, row) {
  if (!inherits(table, "whence_table")) {
    stop("`table` must be a `whence_table` from `whence_table()`, not ", class(table)[1], ".", call. = FALSE)
  }
  n_rows <- nrow(table$value)
  if (!(is.numeric(row) && length(row) == 1 && !is.na(row) && row == round(row) && row >= 1 && row <= n_rows)) {
    stop("`row` must be a whole number from 1 to ", n_rows, ".", call. = FALSE)
  }
  if (is.null(table$keys)) {
    stop(
      "The rows of this table cannot be explained one by one: its last summary step has no group ",
      "columns, or the table does not keep them.",
      call. = FALSE
    )
  }
  row <- as.integer(row)

  replay <- replay_trace(table)
  structure(
    list(
      value = table$value[row, , drop = FALSE],
      stages = group_stages(table, replay, row),
      code = group_code(table, row),
      traced = table$traced,
      reason = group_reason(table, replay),
      label = group_label(table, row),
      table = table,
      row = row,
      replay = replay
    ),
    class = c("whence_group", "whence_trace")
  )
}


#' The waterfall of one group, from the record of the replay
#' @noRd
group_stages <- function(table, replay, row) {
  stages <- table$stages
  n <- vapply(seq_len(nrow(stages)), function(s) {
    codes <- if (s <= length(replay$codes)) replay$codes[[s]] else NULL
    if (is.null(codes)) NA_integer_ else sum(codes %in% row)
  }, integer(1))

  last_summary <- max(c(0L, which(stages$kind %in% aggregate_verbs)))
  previous <- c(NA_integer_, utils::head(n, -1))
  removal <- !stages$kind %in% c("source", aggregate_verbs, "group_by", "ungroup") & seq_along(n) <= last_summary
  delta <- ifelse(removal, n - previous, NA_integer_)

  stages$n <- n
  stages$delta <- as.integer(delta)
  no_percent <- is.na(delta) | delta == 0 | is.na(previous) | previous == 0
  stages$percent <- ifelse(no_percent, NA_real_, 100 * delta / previous)
  stages$groups <- NA_integer_
  if (last_summary > 0) {
    stages$label[[last_summary]] <- "summarised into this row"
  }
  stages
}


#' The code of the table, and a last filter() that keeps this row
#' @noRd
group_code <- function(table, row) {
  conditions <- vapply(table$keys, function(key) {
    value <- table$value[[key]][[row]]
    column <- deparse_one(as.symbol(key))
    if (is.na(value)) {
      return(paste0("is.na(", column, ")"))
    }
    if (is.factor(value)) {
      value <- as.character(value)
    }
    paste(column, "==", deparse_one(value_literal(value)[[1]]))
  }, character(1))
  paste0(table$code, " |>\n  filter(", paste(conditions, collapse = ", "), ")")
}


#' The group of a row as text, such as "cyl = 6, gear = 4"
#' @noRd
group_label <- function(table, row) {
  parts <- vapply(table$keys, function(key) {
    value <- table$value[[key]][[row]]
    paste(key, "=", if (is.na(value)) "NA" else as.character(value))
  }, character(1))
  paste(parts, collapse = ", ")
}


#' Why the counts of a group stop short, or the reason of the table
#' @noRd
group_reason <- function(table, replay) {
  if (replay$ok) {
    return(table$reason)
  }
  sub("^Row attribution unavailable", "Counts for this group are not known", replay$reason)
}


#' @export
format.whence_group <- function(x, ...) {
  lines <- NextMethod()
  lines[[1]] <- paste0("<whence_group> ", x$label)
  lines
}


#' @export
format.whence_table <- function(x, ...) {
  lines <- NextMethod()
  lines[[1]] <- paste0(
    "<whence_table> ", format_count(nrow(x$value)), " rows",
    if (!is.null(x$keys)) paste0(", one for each ", paste(x$keys, collapse = " and ")) else ""
  )
  lines
}
