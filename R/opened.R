# What the viewer has open, for the app.
#
# The browser sets one input when a panel opens or closes: `id` (the output
# id), `row` (the row of a table or a plot, or nothing for a value) and
# `open`. The panel itself never waits for the server; this input only tells
# the app. The leading dot keeps the input out of the inputs that an app
# lists.
opened_input <- ".whence_opened"


#' The panel that the viewer has open
#'
#' `whence_opened()` lets the app react when the viewer opens the panel of an
#' output: for example, to filter other outputs to the row of a table that
#' the viewer is looking at. Call it in a server function or a module server.
#' It gives a reactive expression.
#'
#' The reactive gives `NULL` while no panel of that output is open. While one
#' is open, it gives a list:
#'
#' * `row`: the number of the row of the table, for [whenceTableOutput()] and
#'   [whencePlotOutput()]. `NULL` for [whenceOutput()], and for the panel of a
#'   whole table.
#' * `data`: that row of the table, as a data frame with one row. For
#'   [whenceOutput()], the value. `NULL` for the panel of a whole table.
#'
#' Only one panel is open at a time.
#'
#' @param outputId The id of a whence output, as in the UI. In a module, give
#'   the id without the namespace, as for `output$id`.
#' @param session The session. The default is the current one.
#'
#' @return A reactive expression.
#' @examples
#' if (interactive() && requireNamespace("dplyr", quietly = TRUE)) {
#'   library(shiny)
#'   library(dplyr, warn.conflicts = FALSE)
#'
#'   ui <- fluidPage(
#'     whenceTableOutput("by_cyl", label = "Cars by cylinders"),
#'     textOutput("looking_at")
#'   )
#'   server <- function(input, output, session) {
#'     output$by_cyl <- renderWhenceTable(
#'       mtcars |> group_by(cyl) |> summarise(cars = n())
#'     )
#'     opened <- whence_opened("by_cyl")
#'     output$looking_at <- renderText({
#'       req(opened())
#'       paste("The viewer is looking at", opened()$data$cyl, "cylinders.")
#'     })
#'   }
#'   shinyApp(ui, server)
#' }
#' @export
whence_opened <- function(outputId, session = shiny::getDefaultReactiveDomain()) { # nolint: object_name_linter.
  if (!is.character(outputId) || length(outputId) != 1 || !nzchar(outputId)) {
    stop("`outputId` must be a single string.", call. = FALSE)
  }
  if (is.null(session)) {
    stop("`whence_opened()` must be called in a 'shiny' server function.", call. = FALSE)
  }
  namespaced <- if (is.function(session$ns)) session$ns(outputId) else outputId
  root <- root_session(session)

  shiny::reactive({
    # In a module the output is named with the namespace, and at the root as
    # given. The name that has a trace is the one that is used. (The root
    # session of testServer() adds a prefix in ns() that its outputs do not
    # have, so the rule cannot be ns() alone.)
    traces <- root$userData$.whence_traces
    named <- !is.null(traces) && exists(namespaced, envir = traces, inherits = FALSE)
    full_id <- if (named) namespaced else outputId
    state <- root$input[[opened_input]]
    if (!is.list(state) || !identical(state$id, full_id) || !isTRUE(state$open)) {
      return(NULL)
    }
    opened_state(root, full_id, request_row(state))
  })
}


#' The row and the data of an open panel, from the trace of its output
#'
#' The row comes from the browser, so it is checked against the table.
#' @noRd
opened_state <- function(session, id, row) {
  traces <- session$userData$.whence_traces
  trace <- if (!is.null(traces)) get0(id, envir = traces, inherits = FALSE)
  if (is.null(trace)) {
    return(list(row = row, data = NULL))
  }
  if (!inherits(trace, "whence_table")) {
    return(list(row = NULL, data = trace$value))
  }
  if (is.null(row) || row > nrow(trace$value)) {
    return(list(row = NULL, data = NULL))
  }
  list(row = row, data = trace$value[row, , drop = FALSE])
}
