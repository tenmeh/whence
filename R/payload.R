# What the browser needs to draw a value, a table or a group, and its panel.
#
# The panel opens in the browser with what is here, with no trip to the
# server. A request goes to the server only for rows, or, for a table, for the
# panel of one row.


#' The data for the panel of a trace or a group
#' @noRd
panel_payload <- function(trace) {
  stages <- trace$stages[trace$stages$shown, , drop = FALSE]
  reason <- rows_unavailable_reason(trace)
  removable <- removable_steps(trace)

  list(
    traced = trace$traced,
    reason = trace$reason,
    stages = lapply(seq_len(nrow(stages)), function(i) stage_payload(stages[i, ], removable)),
    code = trace$code,
    rowsAvailable = is.null(reason),
    rowsReason = reason
  )
}


#' The data for a single value: the panel, and the value as text
#' @noRd
explain_payload <- function(trace, format = NULL) {
  value <- trace$value
  c(
    list(
      value = format_value(value, format),
      raw = if (is.numeric(value) || is.logical(value)) unname(value) else as.character(value)
    ),
    panel_payload(trace)
  )
}


#' The data for one row of a table: the panel, and the row as text
#' @noRd
group_payload <- function(group, format = NULL) {
  cells <- format_cells(group$value, format, group$table$keys)
  c(
    list(
      value = group$label,
      rowText = paste(paste(names(cells), "=", unlist(cells)), collapse = ", ")
    ),
    panel_payload(group)
  )
}


#' @noRd
stage_payload <- function(stage, removable = integer(0)) {
  out <- list(label = stage$label, kind = stage$kind, step = stage$step)
  if (!is.na(stage$n)) {
    out$n <- stage$n
    out$nText <- format_count(stage$n)
  }
  if (!is.na(stage$delta) && stage$delta != 0) {
    out$delta <- stage$delta
    out$deltaText <- format_count(abs(stage$delta))
    out$percentText <- format_percent(stage$percent)
    out$removable <- stage$step %in% removable
  }
  if (!is.na(stage$groups)) {
    out$groups <- stage$groups
  }
  out
}


#' The steps whose removed rows can be shown
#'
#' A step that removed rows can show them when the replay can follow every
#' step up to it. This looks at the steps only. It evaluates nothing.
#' @noRd
removable_steps <- function(x) {
  trace <- if (inherits(x, "whence_group")) x$table else x
  if (!trace$traced || id_col %in% names(trace$source_data)) {
    return(integer(0))
  }
  verbs <- vapply(trace$stage_list, stage_verb, character(1))
  followed <- !is.na(verbs) & verbs %in% tracked_verbs
  last_ok <- if (all(followed)) length(verbs) else which(!followed)[1] - 1L
  stages <- x$stages
  stages$step[!is.na(stages$delta) & stages$delta < 0 & stages$step >= 1 & stages$step <= last_ok]
}


#' The cells of a table as text
#'
#' `format` is `NULL`, one function for every number column that is not a
#' group column, or a named list of functions, one for each column to change.
#' @noRd
format_cells <- function(table, format, keys = character(0)) {
  table <- as.data.frame(table)
  out <- lapply(names(table), function(col) {
    values <- table[[col]]
    fn <- if (is.list(format)) {
      format[[col]]
    } else if (is.function(format) && is.numeric(values) && !col %in% keys) {
      format
    }
    if (is.null(fn)) {
      return(vapply(values, function(v) format_value(v), character(1), USE.NAMES = FALSE))
    }
    vapply(values, function(v) format_value(v, fn), character(1), USE.NAMES = FALSE)
  })
  names(out) <- names(table)
  out
}


#' Stop unless `format` fits a table: NULL, a function or a named list
#' @noRd
check_table_format <- function(format) {
  if (is.null(format) || is.function(format)) {
    return(invisible())
  }
  named <- is.list(format) && length(format) > 0 && !is.null(names(format)) && all(nzchar(names(format)))
  if (named && all(vapply(format, is.function, logical(1)))) {
    return(invisible())
  }
  stop(
    "`format` must be `NULL`, a function, or a named list of functions, one for each column.",
    call. = FALSE
  )
}
