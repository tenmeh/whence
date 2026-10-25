#' Parts of a trace
#'
#' `whence_value()` gives the value that the pipeline made.
#' `whence_code()` gives code that makes the value again.
#'
#' In the code, `input$name` and `input[["name"]]` are replaced by their
#' values at the time of the trace. Nothing else is replaced: each other
#' name is written as it is in the pipeline. A wider rule could replace a
#' name that a verb reads as a column, and give wrong code.
#'
#' @param trace A `whence_trace` from [whence_trace()].
#'
#' @return `whence_value()` gives a vector of length one. `whence_code()`
#'   gives a single string.
#' @examplesIf requireNamespace("dplyr", quietly = TRUE)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' input <- list(cyl = 6)
#' trace <- whence_trace(
#'   mtcars |>
#'     filter(cyl == input$cyl) |>
#'     summarise(n = n())
#' )
#' whence_value(trace)
#' cat(whence_code(trace))
#' @export
whence_value <- function(trace) {
  check_trace(trace)
  trace$value
}


#' @rdname whence_value
#' @export
whence_code <- function(trace) {
  check_trace(trace)
  trace$code
}


#' @export
as.data.frame.whence_trace <- function(x, ...) {
  x$stages
}


#' @export
format.whence_trace <- function(x, ...) {
  value <- if (is.data.frame(x$value)) "" else format_value(x$value)
  stages <- x$stages[x$stages$shown, , drop = FALSE]

  lines <- c(
    paste0("<whence_trace> ", value),
    "",
    "Where the rows went"
  )

  if (!x$traced) {
    lines <- c(lines, paste0("  ", x$reason %||% "Pipeline not traced."))
  } else {
    n_text <- format_count(stages$n)
    n_text[is.na(n_text)] <- ""
    pct <- format_percent(stages$percent)
    delta_text <- vapply(seq_along(stages$delta), function(i) {
      d <- stages$delta[[i]]
      if (is.na(d) || d == 0) {
        return("")
      }
      paste0(format_signed(d), " rows", if (nzchar(pct[[i]])) paste0(" (", pct[[i]], ")") else "")
    }, "")
    repeated <- !is.na(stages$added) & stages$added > 0
    delta_text[repeated] <- paste0(
      delta_text[repeated], "  +", format_count(stages$added[repeated]),
      ifelse(stages$added[repeated] == 1L, " row", " rows"),
      " from repeated matches"
    )
    label <- ifelse(stages$kind == "source", stages$label, paste("->", stages$label))
    # The number of each step, as whence_rows(step = ) takes it. A hidden
    # step keeps its number, so the numbers can have gaps.
    step_text <- formatC(stages$step, width = max(nchar(stages$step)))
    width_label <- max(nchar(label))
    width_n <- max(nchar(n_text))
    lines <- c(
      lines,
      sub(
        "\\s+$", "",
        paste0(
          "  ", step_text, " ", formatC(label, width = -width_label), "  ",
          formatC(n_text, width = width_n), "  ", delta_text
        )
      )
    )
  }

  if (x$traced && !is.null(x$reason)) {
    lines <- c(lines, paste0("  ", x$reason))
  }

  c(lines, "", "Reproduce", paste0("  ", strsplit(x$code, "\n", fixed = TRUE)[[1]]))
}


#' @export
print.whence_trace <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}


#' A count with its sign, and a comma for each thousand
#' @noRd
format_signed <- function(d) {
  paste0(if (d < 0) "-" else "+", format_count(abs(d)))
}


#' The display text of a value
#'
#' With no `format` function, a number gets a comma for each thousand and
#' anything else goes through `as.character()`.
#' @noRd
format_value <- function(x, format = NULL) {
  if (!is.null(format)) {
    out <- format(x)
    if (!is.character(out) || length(out) != 1) {
      stop("`format` must return a single string.", call. = FALSE)
    }
    return(out)
  }
  if (is.numeric(x)) {
    return(base::format(x, big.mark = ",", trim = TRUE))
  }
  as.character(x)
}
