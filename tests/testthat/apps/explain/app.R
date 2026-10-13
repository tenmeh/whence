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
managers <- data.frame(region = c("APAC", "EMEA"), manager = c("Kim", "Lou"))

ui <- fluidPage(
  selectInput("region", "Region", c("APAC", "EMEA")),
  tags$p(explainableOutput("total", label = "Total revenue")),
  tags$p(explainableOutput("customers", label = "Customers")),
  tags$p(explainableOutput("joined", label = "Joined")),
  tags$button(id = "after", type = "button", "A control after the outputs")
)

server <- function(input, output, session) {
  output$total <- renderExplainable(
    sales |>
      filter(region == input$region) |>
      filter(year >= 2023) |>
      summarise(total = sum(revenue)),
    format = function(x) paste0("$", format(x, big.mark = ","))
  )
  output$customers <- renderExplainable(
    sales |>
      filter(region == input$region) |>
      distinct(customer) |>
      summarise(n = n())
  )
  # A join ends the chain, so the panel says where it stopped.
  output$joined <- renderExplainable(
    sales |>
      filter(region == input$region) |>
      inner_join(managers, by = "region") |>
      summarise(n = n())
  )
}

shinyApp(ui, server)
