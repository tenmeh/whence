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


#' Count local rows, or run one count query when lazy counting is enabled
#' @noRd
row_count <- function(x, lazy = "stop") {
  if (is_lazy_table(x)) {
    if (lazy == "count") {
      return(lazy_row_count(x))
    }
    stop_lazy_table(x)
  }
  if (is.data.frame(x)) nrow(x) else NA_integer_
}


#' Count the rows of a lazy table, with one query
#'
#' An arrow query keeps a sort on columns that a summary drops, so it is
#' computed and counted by arrow instead. A stingy 'duckplyr' table refuses
#' `ungroup()` even when it has no groups, so the groups are dropped only
#' when there are some.
#' @noRd
lazy_row_count <- function(x) {
  if (inherits(x, c("arrow_dplyr_query", "ArrowTabular", "Dataset"))) {
    return(as.integer(nrow(dplyr::compute(x))))
  }
  if (length(dplyr::group_vars(x)) > 0) {
    x <- dplyr::ungroup(x)
  }
  # The count wraps the step in a subquery, so dbplyr warns that a sort in
  # it is ignored. The warning is about this internal query, not the step.
  count <- suppressWarnings(
    x |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::collect() |>
      dplyr::pull("n")
  )
  # 'dtplyr' summarises an empty table to no rows, not to a count of 0.
  if (length(count) == 0) {
    return(0L)
  }
  as.integer(count)
}


#' The column names of a data frame or a lazy table
#'
#' `names()` of a dbplyr table and of a dtplyr step gives their internal
#' fields, and `colnames()` of an arrow query and of a dtplyr step is empty.
#' @noRd
column_names <- function(x) {
  if (inherits(x, "dtplyr_step")) {
    return(x$vars)
  }
  if (inherits(x, "arrow_dplyr_query")) {
    return(names(x))
  }
  colnames(x)
}


check_lazy <- function(lazy) {
  ok <- is.character(lazy) && length(lazy) == 1L && !is.na(lazy) && lazy %in% c("stop", "count")
  if (!ok) {
    stop('`lazy` must be "stop" or "count", not ', deparse_one(lazy), ".", call. = FALSE)
  }
  invisible(lazy)
}


#' Is `x` a grouped data frame?
#'
#' dplyr keeps the groups of a grouped or rowwise data frame in the
#' `groups` attribute. Reading the attribute directly means that the count
#' pass does not need dplyr. Lazy tables use group_vars() for their metadata.
#' @noRd
is_grouped <- function(x) {
  if (is_lazy_table(x)) {
    return(length(dplyr::group_vars(x)) > 0L)
  }
  is.data.frame(x) &&
    inherits(x, c("grouped_df", "rowwise_df")) &&
    is.data.frame(attr(x, "groups"))
}


#' The number of groups. An ungrouped data frame has one group.
#' @noRd
group_count <- function(x) {
  if (is_lazy_table(x)) {
    return(NA_integer_)
  }
  if (is_grouped(x)) nrow(attr(x, "groups")) else 1L
}


#' The names of the grouping columns
#' @noRd
group_names <- function(x) {
  if (is_lazy_table(x)) {
    return(dplyr::group_vars(x))
  }
  if (!is_grouped(x)) {
    return(character(0))
  }
  setdiff(names(attr(x, "groups")), ".rows")
}


#' Detect lazy tables without asking them to materialise
#' @noRd
is_lazy_table <- function(x) {
  inherits(x, names(lazy_table_kinds))
}

lazy_table_kinds <- list(
  tbl_lazy = list(description = "a `tbl_lazy` from 'dbplyr'", location = "in the database"),
  arrow_dplyr_query = list(description = "an `arrow_dplyr_query` from 'arrow'", location = "in 'arrow'"),
  ArrowTabular = list(description = "an arrow Table (`ArrowTabular`)", location = "in 'arrow'"),
  Dataset = list(description = "a `Dataset` from 'arrow'", location = "in 'arrow'"),
  dtplyr_step = list(description = "a `dtplyr_step` from 'dtplyr'", location = "in 'dtplyr'"),
  prudent_duckplyr_df = list(
    description = paste0(
      "a `prudent_duckplyr_df` from 'duckplyr' that does not allow ",
      "materialisation"
    ),
    location = "in 'duckdb'"
  )
)


lazy_table_kind <- function(x) {
  kind <- names(lazy_table_kinds)[vapply(
    names(lazy_table_kinds), function(kind) inherits(x, kind), logical(1)
  )][[1]]
  lazy_table_kinds[[kind]]
}


stop_lazy_table <- function(x) {
  stop(
    "whence cannot count the rows of a lazy table (", lazy_table_kind(x)$description,
    "). Its steps run only at `collect()`, so there are no row counts ",
    "for each step. Call `collect()` before the steps that you want to explain.",
    call. = FALSE
  )
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
