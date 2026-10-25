# The 'shiny' output.
#
# The render sends the value, the waterfall and the code together. The panel
# thus opens in the browser with no trip to the server. Only "View the rows"
# asks the server for something: it runs the identity pass.


#' An output that explains where its number came from
#'
#' `whenceOutput()` shows a single value. The viewer can click the value
#' (or press Enter or Space on it) to open a panel. The panel shows how many
#' rows each step of the pipeline removed, and the code that makes the value
#' again. A button in the panel shows the rows that make the value.
#'
#' `renderWhence()` makes the value. Give it a pipeline that ends in a
#' single value, as for [whence_trace()]. The pipeline can be inside braces,
#' as in `renderWhence({ ... })`.
#'
#' # The panel
#'
#' The panel is a popover next to the value. On a screen narrower than 576
#' pixels it is a sheet at the bottom of the screen. Escape and a click
#' outside it close it, and the focus then goes back to the value. The panel
#' and the waterfall have no colours of their own: they use `currentColor`
#' and the variables of the Bootstrap theme.
#'
#' These CSS classes are stable, and you can style them: `.whence-value`
#' (the value), `.whence-panel` (the panel), `.whence-backdrop`,
#' `.whence-waterfall` (the list of steps), `.whence-delta` (a count of
#' removed rows) and `.whence-code` (the reproduce code).
#'
#' # Reactivity
#'
#' Each step runs in the environment of the render. It reads inputs and
#' reactive values as the full expression would, so the output updates when
#' they change. `format` runs on the server too, so it can read inputs, for
#' example to choose a currency.
#'
#' @param outputId The output id.
#' @param label Text to show before the value, or `NULL` for none. The panel
#'   uses it in its heading too.
#'
#' @return `whenceOutput()` gives an HTML tag for the UI.
#'   `renderWhence()` gives a render function for the server.
#' @examples
#' whenceOutput("total", label = "Total revenue")
#'
#' if (interactive() && requireNamespace("dplyr", quietly = TRUE)) {
#'   library(shiny)
#'   library(dplyr, warn.conflicts = FALSE)
#'
#'   ui <- fluidPage(
#'     selectInput("cyl", "Cylinders", c(4, 6, 8)),
#'     whenceOutput("mean_mpg", label = "Mean mpg")
#'   )
#'   server <- function(input, output, session) {
#'     output$mean_mpg <- renderWhence(
#'       mtcars |>
#'         filter(cyl == input$cyl) |>
#'         summarise(mpg = mean(mpg)),
#'       format = function(x) paste(round(x, 1), "mpg")
#'     )
#'   }
#'   shinyApp(ui, server)
#' }
#' @export
# The names follow the xOutput() and renderX() pattern of 'shiny'.
whenceOutput <- function(outputId, label = NULL) { # nolint: object_name_linter.
  if (!is.character(outputId) || length(outputId) != 1 || !nzchar(outputId)) {
    stop("`outputId` must be a single string.", call. = FALSE)
  }
  check_string_or_null(label, "label")

  htmltools::attachDependencies(
    htmltools::tags$div(
      id = outputId,
      class = "whence-output",
      `data-label` = label,
      if (!is.null(label)) htmltools::tags$span(class = "whence-label", label),
      htmltools::tags$span(class = "whence-slot")
    ),
    whence_dependency()
  )
}


#' @rdname whenceOutput
#' @inheritParams whence_trace
#' @param expr A pipeline that ends in a single value.
#' @param format A function that takes the value and gives the text to show.
#'   A formatter of 'scales' works, for example `scales::label_dollar()`, and
#'   so does `function(x) paste0(round(x), " kg")`. With `NULL`, a number gets
#'   a comma for each thousand, and any other value goes through
#'   `as.character()`.
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` already quoted? Use `TRUE` when you give an
#'   expression made with `quote()`.
#' @param outputArgs A list of arguments for `whenceOutput()`, such as
#'   `label`, when the render is used in an R Markdown document with
#'   `runtime: shiny`.
#' @export
renderWhence <- function(expr, format = NULL, lazy = getOption("whence.lazy", "stop"), # nolint: object_name_linter.
                         env = parent.frame(), quoted = FALSE,
                         outputArgs = list()) { # nolint: object_name_linter. The name is that of shiny.
  check_lazy(lazy)
  if (!is.null(format) && !is.function(format)) {
    stop("`format` must be a function or `NULL`, not ", class(format)[1], ".", call. = FALSE)
  }
  check_render_args(env, quoted, outputArgs)
  if (!quoted) {
    expr <- substitute(expr)
  }

  shiny::createRenderFunction(
    function() trace_expr(expr, env, lazy = lazy),
    transform = function(value, session, name, ...) {
      remember_trace(session, name, value, format)
      explain_payload(value, format)
    },
    outputFunc = whenceOutput,
    outputArgs = outputArgs,
    cacheHint = list(label = "renderWhence", userExpr = expr)
  )
}


#' Check the arguments that every render function of whence shares
#' @noRd
check_render_args <- function(env, quoted, outputArgs) { # nolint: object_name_linter.
  if (!is.environment(env)) {
    stop("`env` must be an environment, not ", class(env)[1], ".", call. = FALSE)
  }
  if (!(is.logical(quoted) && length(quoted) == 1 && !is.na(quoted))) {
    stop("`quoted` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (!is.list(outputArgs)) {
    stop("`outputArgs` must be a list, not ", class(outputArgs)[1], ".", call. = FALSE)
  }
  invisible()
}


#' The HTML dependency of the output
#' @noRd
whence_dependency <- function() {
  htmltools::htmlDependency(
    name = "whence",
    version = as.character(utils::packageVersion("whence")),
    src = c(file = system.file("www", package = "whence")),
    script = "whence.js",
    stylesheet = "whence.css"
  )
}
