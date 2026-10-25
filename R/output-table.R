#' A summary table whose rows explain where they came from
#'
#' `whenceTableOutput()` shows a table, such as the result of
#' `group_by(region) |> summarise(total = sum(revenue))`. The viewer clicks
#' the first cell of a row (or presses Enter or Space on it) to open the panel
#' of that row: how many rows of its group each step removed, the code that
#' makes the row, and the rows of the group. A link under the table opens the
#' panel of the table as a whole.
#'
#' `renderWhenceTable()` makes the table. Give it a pipeline that ends in a
#' summary, as for [whence_table()]. The table itself is sent with the render.
#' The panel of a row needs one trip to the server, because it replays the
#' steps for its group.
#'
#' The rows can be explained when the group columns of the last summary step
#' are in the table. The table shows at most 500 rows.
#'
#' @inheritParams whenceOutput
#' @return `whenceTableOutput()` gives an HTML tag for the UI.
#'   `renderWhenceTable()` gives a render function for the server.
#' @examples
#' whenceTableOutput("by_region", label = "Revenue by region")
#'
#' if (interactive() && requireNamespace("dplyr", quietly = TRUE)) {
#'   library(shiny)
#'   library(dplyr, warn.conflicts = FALSE)
#'
#'   ui <- fluidPage(whenceTableOutput("by_cyl", label = "Mean mpg by cylinders"))
#'   server <- function(input, output, session) {
#'     output$by_cyl <- renderWhenceTable(
#'       mtcars |>
#'         filter(hp > 90) |>
#'         group_by(cyl) |>
#'         summarise(mpg = mean(mpg)),
#'       format = list(mpg = function(x) format(round(x, 1)))
#'     )
#'   }
#'   shinyApp(ui, server)
#' }
#' @export
whenceTableOutput <- function(outputId, label = NULL) { # nolint: object_name_linter.
  if (!is.character(outputId) || length(outputId) != 1 || !nzchar(outputId)) {
    stop("`outputId` must be a single string.", call. = FALSE)
  }
  check_string_or_null(label, "label")

  htmltools::attachDependencies(
    htmltools::tags$div(
      id = outputId,
      class = "whence-output whence-table-output",
      `data-label` = label,
      if (!is.null(label)) htmltools::tags$div(class = "whence-label", label),
      htmltools::tags$div(class = "whence-slot")
    ),
    whence_dependency()
  )
}


#' @rdname whenceTableOutput
#' @param expr A pipeline that ends in a summary table.
#' @param format `NULL`, a function for every number column that is not a
#'   group column, or a named list of functions, one for each column to
#'   change, such as `list(total = scales::label_dollar())`.
#' @inheritParams renderWhence
#' @export
renderWhenceTable <- function(expr, format = NULL, # nolint: object_name_linter.
                              lazy = getOption("whence.lazy", "stop"),
                              env = parent.frame(), quoted = FALSE,
                              outputArgs = list()) { # nolint: object_name_linter. The name is that of shiny.
  check_lazy(lazy)
  check_table_format(format)
  check_render_args(env, quoted, outputArgs)
  if (!quoted) {
    expr <- substitute(expr)
  }

  shiny::createRenderFunction(
    function() trace_expr(expr, env, mode = "table", lazy = lazy),
    transform = function(value, session, name, ...) {
      remember_trace(session, name, value, format)
      table_payload(value, format)
    },
    outputFunc = whenceTableOutput,
    outputArgs = outputArgs,
    cacheHint = list(label = "renderWhenceTable", userExpr = expr)
  )
}


# The most rows of a summary table that the browser shows.
max_table_rows <- 500L


#' The data that the browser needs to draw a table and its panels
#' @noRd
table_payload <- function(trace, format = NULL) {
  value <- as.data.frame(trace$value)
  shown <- utils::head(value, max_table_rows)
  cells <- format_cells(shown, format, trace$keys)
  list(
    kind = "table",
    columns = names(value),
    rows = lapply(seq_len(nrow(shown)), function(i) unname(lapply(cells, `[[`, i))),
    total = nrow(value),
    totalText = format_count(nrow(value)),
    shown = nrow(shown),
    explainable = !is.null(trace$keys),
    keysReason = if (is.null(trace$keys)) {
      "The rows cannot be explained one by one: the table does not keep the group columns of its last summary."
    },
    panel = c(list(value = paste(format_count(nrow(value)), if (nrow(value) == 1) "row" else "rows")),
              panel_payload(trace))
  )
}
