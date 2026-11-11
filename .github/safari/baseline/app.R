# A baseline for #15: Shiny and dplyr, the same data and the same kind of
# pipeline as the demo, with no whence. If this also runs out of stack in
# Safari, the limit is in shinylive, webR or dplyr, not in whence.
library(shiny)
library(dplyr, warn.conflicts = FALSE)

set.seed(2026)
n_rows <- 2000
sales <- data.frame(
  customer = sample(c("Acme Corp", "Bolt Ltd", "Cove & Co"), n_rows, replace = TRUE),
  region = sample(c("APAC", "EMEA", "Americas"), n_rows, replace = TRUE),
  year = sample(2020:2025, n_rows, replace = TRUE),
  revenue = round(stats::rlnorm(n_rows, meanlog = 7, sdlog = 1)),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  selectInput("region", "Region", sort(unique(sales$region))),
  textOutput("revenue"),
  tableOutput("by_customer")
)

server <- function(input, output, session) {
  output$revenue <- renderText({
    sales |>
      filter(region == input$region) |>
      filter(year >= 2023) |>
      summarise(total = sum(revenue)) |>
      pull(total)
  })
  output$by_customer <- renderTable({
    sales |>
      filter(region == input$region) |>
      group_by(customer) |>
      summarise(revenue = sum(revenue))
  })
}

shinyApp(ui, server)
