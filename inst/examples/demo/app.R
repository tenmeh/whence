# A demo of whence. Run it with:
#   shiny::runApp(system.file("examples", "demo", package = "whence"))
#
# Click a number to see where its rows went.

library(shiny)
library(whence)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("This demo needs dplyr. Install it with install.packages(\"dplyr\").", call. = FALSE)
}
library(dplyr, warn.conflicts = FALSE)

# --- Data ---------------------------------------------------------------------
# Made up, and the same at each start.
set.seed(2026)
n_rows <- 18442
sales <- data.frame(
  customer = sample(
    c("Acme Corp", "Bolt Ltd", "Cove & Co", "Dune Retail", "Elm Partners"),
    n_rows,
    replace = TRUE
  ),
  region = sample(c("APAC", "EMEA", "Americas"), n_rows, replace = TRUE),
  year = sample(2020:2025, n_rows, replace = TRUE),
  channel = sample(c("Online", "Partner", "Direct"), n_rows, replace = TRUE),
  revenue = round(stats::rlnorm(n_rows, meanlog = 7, sdlog = 1)),
  stringsAsFactors = FALSE
)

dollars <- function(x) paste0("$", format(round(x), big.mark = ","))

# --- UI ---------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("whence demo"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Region", sort(unique(sales$region))),
      sliderInput("from", "From year", 2020, 2025, 2023, sep = ""),
      checkboxGroupInput(
        "channel", "Channel",
        sort(unique(sales$channel)),
        selected = sort(unique(sales$channel))
      )
    ),
    mainPanel(
      tags$p("Click a number to see where it came from."),
      tags$h3(whenceOutput("revenue", label = "Revenue")),
      tags$h3(whenceOutput("orders", label = "Orders")),
      tags$h3(whenceOutput("top_share", label = "Share of the top customer (%)")),
      tags$h3(whenceOutput("customers", label = "Customers"))
    )
  )
)

# --- Server -------------------------------------------------------------------
server <- function(input, output, session) {
  output$revenue <- renderWhence(
    sales |>
      filter(region == input$region) |>
      filter(year >= input$from) |>
      filter(channel %in% input$channel) |>
      summarise(total = sum(revenue)),
    format = dollars
  )

  output$orders <- renderWhence(
    sales |>
      filter(region == input$region, year >= input$from) |>
      filter(channel %in% input$channel) |>
      count()
  )

  output$top_share <- renderWhence(
    sales |>
      filter(region == input$region, year >= input$from) |>
      group_by(customer) |>
      summarise(total = sum(revenue)) |>
      mutate(share = round(100 * total / sum(total), 1)) |>
      slice_max(share, n = 1, with_ties = FALSE) |>
      pull(share)
  )

  # distinct() ends row attribution. The waterfall still works.
  output$customers <- renderWhence(
    sales |>
      filter(region == input$region, year >= input$from) |>
      distinct(customer) |>
      count()
  )
}

shinyApp(ui, server)
