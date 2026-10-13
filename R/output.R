# The 'shiny' output.
#
# The render sends the value, the waterfall and the code together. The panel
# thus opens in the browser with no trip to the server. Only "View the rows"
# asks the server for something: it runs the identity pass.


# The `format` functions of explainableOutput(), by output id.
#
# A UI function cannot send an R function to the server. explainableOutput()
# thus records `format` here, and the render looks it up by the output id.
# In a module the UI id and the render name are both the full namespaced id,
# so they agree.
format_registry <- new.env(parent = emptyenv())

# The most rows that "View the rows" sends to the browser.
max_rows_sent <- 10000L


#' An output that explains where its number came from
#'
#' `explainableOutput()` shows a single value. The viewer can click the value
#' (or press Enter or Space on it) to open a panel. The panel shows how many
#' rows each step of the pipeline removed, and the code that makes the value
#' again. A button in the panel shows the rows that make the value.
#'
#' `renderExplainable()` makes the value. Give it a pipeline that ends in a
#' single value, as for [whence_trace()].
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
#' they change.
#'
#' @param outputId The output id.
#' @param label Text to show before the value, or `NULL` for none. The panel
#'   uses it in its heading too.
#' @param format A function that takes the value and gives the text to show,
#'   for example `function(x) paste0("$", format(x, big.mark = ","))`. With
#'   `NULL`, a number gets a comma for each thousand, and any other value
#'   goes through `as.character()`.
#'
#' @return `explainableOutput()` gives an HTML tag for the UI.
#'   `renderExplainable()` gives a render function for the server.
#' @examples
#' explainableOutput("total", label = "Total revenue")
#'
#' if (interactive() && requireNamespace("dplyr", quietly = TRUE)) {
#'   library(shiny)
#'   library(dplyr, warn.conflicts = FALSE)
#'
#'   ui <- fluidPage(
#'     selectInput("cyl", "Cylinders", c(4, 6, 8)),
#'     explainableOutput("mean_mpg", label = "Mean mpg")
#'   )
#'   server <- function(input, output, session) {
#'     output$mean_mpg <- renderExplainable(
#'       mtcars |>
#'         filter(cyl == input$cyl) |>
#'         summarise(mpg = round(mean(mpg), 1))
#'     )
#'   }
#'   shinyApp(ui, server)
#' }
#' @export
# The names follow the xOutput() and renderX() pattern of 'shiny'.
explainableOutput <- function(outputId, label = NULL, format = NULL) { # nolint: object_name_linter.
  if (!is.character(outputId) || length(outputId) != 1 || !nzchar(outputId)) {
    stop("`outputId` must be a single string.", call. = FALSE)
  }
  check_string_or_null(label, "label")
  if (!is.null(format) && !is.function(format)) {
    stop("`format` must be a function or `NULL`.", call. = FALSE)
  }

  if (is.null(format)) {
    if (exists(outputId, envir = format_registry, inherits = FALSE)) {
      rm(list = outputId, envir = format_registry)
    }
  } else {
    assign(outputId, format, envir = format_registry)
  }

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


#' @rdname explainableOutput
#' @param expr A pipeline that ends in a single value.
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` already quoted? Use `TRUE` when you give an
#'   expression made with `quote()`.
#' @export
renderExplainable <- function(expr, env = parent.frame(), quoted = FALSE) { # nolint: object_name_linter.
  if (!quoted) {
    expr <- substitute(expr)
  }
  force(env)

  shiny::createRenderFunction(
    function() trace_expr(expr, env),
    transform = function(value, session, name, ...) {
      remember_trace(session, name, value)
      explain_payload(value, format_registry[[name]])
    },
    outputFunc = explainableOutput,
    cacheHint = list(label = "renderExplainable", userExpr = expr)
  )
}


#' The data that the browser needs to show the value and the panel
#' @noRd
explain_payload <- function(trace, format = NULL) {
  value <- trace$value
  stages <- trace$stages[trace$stages$shown, , drop = FALSE]
  reason <- rows_unavailable_reason(trace)

  list(
    value = format_value(value, format),
    raw = if (is.numeric(value) || is.logical(value)) unname(value) else as.character(value),
    traced = trace$traced,
    stages = lapply(seq_len(nrow(stages)), function(i) stage_payload(stages[i, ])),
    code = trace$code,
    rowsAvailable = is.null(reason),
    rowsReason = reason
  )
}


#' @noRd
stage_payload <- function(stage) {
  out <- list(label = stage$label, kind = stage$kind)
  if (!is.na(stage$n)) {
    out$n <- stage$n
    out$nText <- format_count(stage$n)
  }
  if (!is.na(stage$delta) && stage$delta != 0) {
    out$delta <- stage$delta
    out$deltaText <- format_count(abs(stage$delta))
  }
  if (!is.na(stage$groups)) {
    out$groups <- stage$groups
  }
  out
}


#' Keep the latest trace of an output, and answer "View the rows"
#'
#' The observer for the rows request is made once for each output, at its
#' first render. It reads the trace that is current when the request comes.
#' @noRd
remember_trace <- function(session, name, trace) {
  key <- paste0(".whence-", name)
  state <- session$userData[[key]]
  if (is.null(state)) {
    state <- new.env(parent = emptyenv())
    session$userData[[key]] <- state
  }
  state$trace <- trace

  if (is.null(state$observer)) {
    state$observer <- shiny::observeEvent(
      session$input[[paste0(name, "_whence_rows")]],
      send_rows(session, name, state$trace),
      ignoreInit = TRUE,
      domain = session
    )
  }
  invisible(state)
}


#' Run the identity pass and send the rows to the browser
#' @noRd
send_rows <- function(session, name, trace, has_reactable = requireNamespace("reactable", quietly = TRUE)) {
  result <- tryCatch(
    identity_pass(trace),
    error = function(e) {
      list(rows = NULL, reason = "Row attribution unavailable: a step failed.")
    }
  )

  if (is.null(result$rows)) {
    message <- list(id = name, ok = FALSE, message = result$reason)
  } else {
    rows <- result$rows
    total <- nrow(rows)
    cap <- rows_cap(has_reactable)
    if (total > cap) {
      rows <- rows[seq_len(cap), , drop = FALSE]
    }
    content <- rows_content(rows, has_reactable)
    message <- list(
      id = name,
      ok = TRUE,
      total = total,
      totalText = format_count(total),
      shown = nrow(rows),
      shownText = format_count(nrow(rows)),
      html = content$html,
      deps = content$deps
    )
  }

  session$sendCustomMessage("whence:rows", message)
}


#' The rows as HTML, with their dependencies
#'
#' 'reactable' gives a table with pages and a search box. Without it the
#' rows are a plain HTML table.
#' @noRd
rows_content <- function(rows, has_reactable = requireNamespace("reactable", quietly = TRUE)) {
  rows <- as.data.frame(rows)
  table <- if (has_reactable) {
    reactable::reactable(
      rows,
      compact = TRUE,
      searchable = TRUE,
      defaultPageSize = 10,
      highlight = TRUE
    )
  } else {
    plain_table(rows)
  }

  rendered <- htmltools::renderTags(table)
  deps <- htmltools::resolveDependencies(rendered$dependencies)
  list(
    html = as.character(rendered$html),
    deps = lapply(deps, shiny::createWebDependency)
  )
}


#' @noRd
rows_cap <- function(has_reactable) {
  if (has_reactable) max_rows_sent else 200L
}


#' @noRd
plain_table <- function(rows) {
  shown <- utils::head(rows, rows_cap(FALSE))
  cells <- lapply(shown, function(col) format(col, trim = TRUE))
  htmltools::tags$div(
    class = "whence-rows-table",
    htmltools::tags$table(
      class = "table table-sm",
      htmltools::tags$thead(
        htmltools::tags$tr(lapply(names(rows), htmltools::tags$th))
      ),
      htmltools::tags$tbody(
        lapply(seq_len(nrow(shown)), function(i) {
          htmltools::tags$tr(lapply(cells, function(col) htmltools::tags$td(col[[i]])))
        })
      )
    )
  )
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
