#' A plot whose bars and points explain where they came from
#'
#' `whencePlotOutput()` shows a plot of a summary table. The viewer clicks a
#' bar or a point to open the panel of that row of the table: how many rows of
#' its group each step removed, the code that makes the row, and the rows of
#' the group. Under the plot, "Show the data" gives the same table, so a
#' keyboard user can open each panel too.
#'
#' `renderWhencePlot()` makes the plot. `expr` is a pipeline that ends in a
#' summary, as for [whence_table()]. `plot` is a function that takes that
#' summary and gives a 'ggplot2' plot. A click finds its row through the
#' aesthetics of the plot: on a discrete x axis, by the bar under the click;
#' otherwise, by the nearest point. The `x` (and `y`) aesthetics must be
#' columns of the summary.
#'
#' @inheritParams whenceOutput
#' @param width,height The size of the plot, as for [shiny::plotOutput()].
#' @return `whencePlotOutput()` gives an HTML tag for the UI.
#'   `renderWhencePlot()` gives a render function for the server.
#' @examples
#' whencePlotOutput("by_cyl", label = "Mean mpg by cylinders")
#'
#' if (interactive() && requireNamespace("dplyr", quietly = TRUE) &&
#'   requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(shiny)
#'   library(dplyr, warn.conflicts = FALSE)
#'   library(ggplot2)
#'
#'   ui <- fluidPage(whencePlotOutput("by_cyl", label = "Mean mpg by cylinders"))
#'   server <- function(input, output, session) {
#'     output$by_cyl <- renderWhencePlot(
#'       mtcars |>
#'         filter(hp > 90) |>
#'         group_by(cyl = factor(cyl)) |>
#'         summarise(mpg = mean(mpg)),
#'       plot = function(data) ggplot(data, aes(cyl, mpg)) + geom_col()
#'     )
#'   }
#'   shinyApp(ui, server)
#' }
#' @export
whencePlotOutput <- function(outputId, label = NULL, width = "100%", height = "400px") { # nolint: object_name_linter.
  if (!is.character(outputId) || length(outputId) != 1 || !nzchar(outputId)) {
    stop("`outputId` must be a single string.", call. = FALSE)
  }
  check_string_or_null(label, "label")

  htmltools::attachDependencies(
    htmltools::tags$div(
      class = "whence-plot",
      `data-whence-id` = outputId,
      `data-label` = label,
      if (!is.null(label)) htmltools::tags$div(class = "whence-label", label),
      shiny::plotOutput(
        outputId,
        width = width,
        height = height,
        click = shiny::clickOpts(id = paste0(outputId, plot_click_suffix))
      ),
      htmltools::tags$details(
        class = "whence-plot-data",
        htmltools::tags$summary("Show the data"),
        htmltools::tags$div(class = "whence-plot-table", id = paste0(outputId, "-whence-table"))
      )
    ),
    whence_dependency()
  )
}


#' @rdname whencePlotOutput
#' @param expr A pipeline that ends in a summary table.
#' @param plot A function that takes the summary table and gives a plot.
#' @param format As for [renderWhenceTable()]: for the table under the plot,
#'   and the panels.
#' @param res,alt As for [shiny::renderPlot()].
#' @inheritParams renderWhence
#' @export
renderWhencePlot <- function(expr, plot, format = NULL, width = "auto", height = "auto", # nolint: object_name_linter.
                             res = 72, alt = NA, lazy = getOption("whence.lazy", "stop"),
                             env = parent.frame(), quoted = FALSE,
                             outputArgs = list()) { # nolint: object_name_linter. The name is that of shiny.
  check_lazy(lazy)
  if (!is.function(plot)) {
    stop("`plot` must be a function that takes the summary table, not ", class(plot)[1], ".", call. = FALSE)
  }
  check_table_format(format)
  check_render_args(env, quoted, outputArgs)
  if (!quoted) {
    expr <- substitute(expr)
  }

  # renderPlot() evaluates an expression. This one calls a function that
  # traces the table, keeps it for the clicks, sends the table for "Show the
  # data", and draws the plot.
  draw_env <- new.env(parent = env)
  draw_env$.whence_draw <- function() {
    trace <- trace_expr(expr, env, mode = "table", lazy = lazy)
    drawn <- plot(trace$value)
    # The x and y of each row, from the plot itself. A click is matched to
    # these, so the text of the click data is never evaluated.
    trace$plot_values <- plot_values(drawn, trace$value)
    session <- shiny::getDefaultReactiveDomain()
    name <- if (!is.null(session)) shiny::getCurrentOutputInfo(session)$name
    if (!is.null(name)) {
      remember_trace(session, name, trace, format)
      root_session(session)$sendCustomMessage(
        "whence:table",
        list(id = name, table = table_payload(trace, format))
      )
    }
    # A ggplot must be returned, not printed: renderPlot() then builds it and
    # keeps its coordinates, so a click knows the axes and the levels.
    drawn
  }

  shiny::renderPlot(
    quote(.whence_draw()),
    width = width,
    height = height,
    res = res,
    alt = alt,
    env = draw_env,
    quoted = TRUE,
    outputArgs = outputArgs
  )
}


# The suffix of the click input of a whence plot. The browser sends each
# click to the server as a "point" request instead.
plot_click_suffix <- "_whence_click"


#' The row of a summary table under a click on its plot, or NA
#'
#' `values` holds the x and y of each row, from plot_values(), so a
#' computed aesthetic such as `reorder(region, total)` finds its rows too.
#' Without it, x and y must be plain columns.
#'
#' On a discrete x axis, the bar under the click is the level of x at the
#' rounded x of the click. When more than one row has that level, as in a
#' stacked or dodged chart, the row whose y is nearest to the click wins.
#' On a continuous axis, the nearest point within 15 pixels wins.
#' @noRd
clicked_row <- function(data, click, values = NULL) {
  data <- as.data.frame(data)
  if (!is.list(click) || !is.numeric(unlist(click$x)) || length(unlist(click$x)) != 1) {
    return(NA_integer_)
  }
  x <- values$x %||% aesthetic_values(data, unlist(click$mapping$x))
  y <- values$y %||% aesthetic_values(data, unlist(click$mapping$y))
  if (is.null(x)) {
    return(NA_integer_)
  }

  # A discrete axis: x for upright bars, y for horizontal bars.
  x_limits <- unlist(click$domain$discrete_limits$x)
  y_limits <- unlist(click$domain$discrete_limits$y)
  if (!is.null(x_limits)) {
    return(discrete_row(x, x_limits, click$x, y, click$y))
  }
  if (!is.null(y_limits)) {
    return(discrete_row(y, y_limits, click$y, x, click$x))
  }

  if (!is.numeric(y)) {
    return(NA_integer_)
  }
  points <- data.frame(.whence_x = x, .whence_y = y)
  near <- tryCatch(
    shiny::nearPoints(
      points, click,
      xvar = ".whence_x", yvar = ".whence_y",
      threshold = 15, maxpoints = 1, allRows = TRUE
    ),
    error = function(e) NULL
  )
  hit <- if (!is.null(near)) which(near$selected_) else integer(0)
  if (length(hit) > 0) hit[[1]] else NA_integer_
}


#' The row on a discrete axis: the level under the click, and when more
#' than one row has it, the row nearest to the click on the other axis
#' @noRd
discrete_row <- function(values, limits, at, other, other_at) {
  i <- round(as.numeric(at))
  if (length(i) != 1 || is.na(i) || i < 1 || i > length(limits)) {
    return(NA_integer_)
  }
  candidates <- which(as.character(values) == limits[[i]])
  if (length(candidates) > 1 && is.numeric(other)) {
    return(candidates[which.min(abs(other[candidates] - as.numeric(other_at)))])
  }
  if (length(candidates) > 0) candidates[[1]] else NA_integer_
}


#' The x and y of each row of the table, from the aesthetics of a ggplot
#'
#' The mappings of the plot, and of its first layer that draws the table,
#' are evaluated on the table, as ggplot does. A layer with data of its own
#' is skipped, and a layer with `inherit.aes = FALSE` does not take the
#' mappings of the plot. So `aes(reorder(region, total), total)` gives the
#' level of each row. The mappings come from the code of the app, not from
#' the browser. Anything that fails gives NULL, and a click then falls back
#' to a plain column name.
#' @noRd
plot_values <- function(drawn, data) {
  if (!inherits(drawn, "ggplot")) {
    return(NULL)
  }
  data <- as.data.frame(data)
  mapping <- as.list(drawn$mapping)
  own_data <- vapply(drawn$layers, function(layer) !inherits(layer$data, "waiver"), logical(1))
  if (length(drawn$layers) > 0 && all(own_data)) {
    # No layer draws the table, so no click can be matched to its rows.
    return(NULL)
  }
  if (length(drawn$layers) > 0) {
    layer <- drawn$layers[[which(!own_data)[1]]]
    if (isFALSE(layer$inherit.aes)) {
      mapping <- list()
    }
    layer_mapping <- as.list(layer$mapping)
    mapping[names(layer_mapping)] <- layer_mapping
  }
  evaluate <- function(aesthetic) {
    if (is.null(aesthetic)) {
      return(NULL)
    }
    values <- tryCatch(rlang::eval_tidy(aesthetic, data = data), error = function(e) NULL)
    if (is.null(values) || length(values) != nrow(data)) NULL else values
  }
  list(x = evaluate(mapping$x), y = evaluate(mapping$y))
}


#' The values of a plain column named by the click data, or NULL
#'
#' Only an exact column name is used. The text is never evaluated: it comes
#' from the browser.
#' @noRd
aesthetic_values <- function(data, text) {
  if (is.null(text) || !is.character(text) || length(text) != 1 || !text %in% names(data)) {
    return(NULL)
  }
  data[[text]]
}
