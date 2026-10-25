# The app for the browser tests in test-output.R.
library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(whence)

sales <- data.frame(
  customer = rep(c("Acme", "Bolt", "Cove", "Dune"), times = 5),
  region = rep(c("APAC", "EMEA"), each = 10),
  year = rep(2021:2025, times = 4),
  revenue = seq(100, 2000, by = 100),
  stringsAsFactors = FALSE
)
managers <- data.frame(region = c("APAC", "APAC", "EMEA"), manager = c("Kim", "Pat", "Lou"))

ui <- fluidPage(
  selectInput("region", "Region", c("APAC", "EMEA")),
  numericInput("plot_from", "Plot from year", 2023),
  tags$p(whenceOutput("total", label = "Total revenue")),
  tags$p(whenceOutput("customers", label = "Customers")),
  tags$div(id = "joined-wrap", whenceOutput("joined", label = "Joined")),
  tags$p(whenceOutput("joined_one", label = "One repeated match")),
  uiOutput("dynamic"),
  whenceTableOutput("by_region", label = "Revenue by region"),
  whencePlotOutput("plot_region", label = "Revenue by region", width = "400px", height = "300px"),
  actionButton("drop", "Remove the joined output"),
  actionButton("rebuild", "Rebuild the dynamic output"),
  tags$button(id = "after", type = "button", "A control after the outputs")
)

server <- function(input, output, session) {
  output$total <- renderWhence(
    sales |>
      filter(region == input$region) |>
      filter(year >= 2023) |>
      summarise(total = sum(revenue)),
    format = function(x) paste0("$", format(x, big.mark = ","))
  )
  output$customers <- renderWhence(
    sales |>
      filter(region == input$region) |>
      distinct(customer) |>
      summarise(n = n())
  )
  # renderUI() makes the output again, with the same id, at each rebuild.
  output$dynamic <- renderUI({
    input$rebuild
    tags$p(whenceOutput("dynamic_total", label = "Dynamic"))
  })
  output$dynamic_total <- renderWhence(
    sales |> filter(region == "EMEA") |> summarise(n = n())
  )
  observeEvent(input$drop, removeUI("#joined-wrap"))

  by_region <- quote(
    sales |>
      filter(year >= 2023) |>
      group_by(region) |>
      summarise(total = sum(revenue))
  )
  output$by_region <- renderWhenceTable(
    by_region,
    format = list(total = function(x) paste0("$", format(x, big.mark = ","))),
    quoted = TRUE
  )
  output$plot_region <- renderWhencePlot(
    sales |>
      filter(year >= input$plot_from) |>
      group_by(region) |>
      summarise(total = sum(revenue)),
    # A computed aesthetic, as for bars sorted by their height.
    plot = function(data) {
      ggplot2::ggplot(data, ggplot2::aes(reorder(region, total), total)) + ggplot2::geom_col()
    }
  )

  # The traces that the server holds, for the clean-up tests, and the row
  # of the table whose panel is open, for whence_opened().
  opened <- whence_opened("by_region")
  exportTestValues(
    traces = sort(ls(session$userData$.whence_traces)),
    opened_row = if (is.null(opened())) NA_integer_ else opened()$row,
    opened_region = if (is.null(opened())) NA_character_ else opened()$data$region
  )

  # Repeated matches have their own count, separate from removed rows.
  output$joined <- renderWhence(
    sales |>
      filter(region == input$region) |>
      inner_join(managers, by = "region", relationship = "many-to-many") |>
      summarise(n = n())
  )
  output$joined_one <- renderWhence(
    sales |>
      filter(revenue == 100) |>
      inner_join(managers, by = "region", relationship = "many-to-many") |>
      summarise(n = n())
  )
}

shinyApp(ui, server)
