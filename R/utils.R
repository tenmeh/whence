`%||%` <- function(x, y) if (is.null(x)) y else x


#' Deparse an expression to one string
#' @noRd
deparse_one <- function(expr, collapse = " ") {
  text <- deparse(expr, width.cutoff = 500L, backtick = TRUE)
  if (collapse == " ") {
    text <- trimws(text)
  }
  paste(text, collapse = collapse)
}


#' Format a count with a comma for each thousand
#' @noRd
format_count <- function(n) {
  out <- format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
  out[is.na(n)] <- NA_character_
  out
}


#' The number of rows of a data frame, or `NA` for anything else
#' @noRd
row_count <- function(x) {
  if (is.data.frame(x)) nrow(x) else NA_integer_
}


#' Is `x` a grouped data frame?
#'
#' dplyr keeps the groups of a grouped or rowwise data frame in the
#' `groups` attribute. Reading the attribute directly means that the count
#' pass does not need dplyr.
#' @noRd
is_grouped <- function(x) {
  is.data.frame(x) &&
    inherits(x, c("grouped_df", "rowwise_df")) &&
    is.data.frame(attr(x, "groups"))
}


#' The number of groups. An ungrouped data frame has one group.
#' @noRd
group_count <- function(x) {
  if (is_grouped(x)) nrow(attr(x, "groups")) else 1L
}


#' The names of the grouping columns
#' @noRd
group_names <- function(x) {
  if (!is_grouped(x)) {
    return(character(0))
  }
  setdiff(names(attr(x, "groups")), ".rows")
}


#' Stop with a clear message when the data lives in a database
#' @noRd
check_not_lazy <- function(x) {
  if (inherits(x, "tbl_lazy")) {
    stop(
      "whence cannot trace a database-backed pipeline (a `tbl_lazy`). ",
      "Its steps become SQL and run only at `collect()`, so there are no ",
      "row counts to show for each step. Call `collect()` before the ",
      "steps that you want to explain.",
      call. = FALSE
    )
  }
  invisible(x)
}


#' Stop unless `x` is a `whence_trace`
#' @noRd
check_trace <- function(x, arg = "trace") {
  if (!inherits(x, "whence_trace")) {
    stop(
      "`", arg, "` must be a `whence_trace` from `whence_trace()`, not ",
      class(x)[1], ".",
      call. = FALSE
    )
  }
  invisible(x)
}


#' Stop unless `x` is `NULL` or one string
#' @noRd
check_string_or_null <- function(x, arg) {
  if (!is.null(x) && !(is.character(x) && length(x) == 1 && !is.na(x))) {
    stop("`", arg, "` must be a single string or `NULL`.", call. = FALSE)
  }
  invisible(x)
}
