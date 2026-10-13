# A demo of whence. Run it with:
#   shiny::runApp(system.file("examples", "demo", package = "whence"))
#
# Click a number, a row of the table, or a bar of the plot to see where its
# rows went.

library(shiny)
library(whence)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("This demo needs dplyr. Install it with install.packages(\"dplyr\").", call. = FALSE)
}
library(dplyr, warn.conflicts = FALSE)

# The plot needs ggplot2. Without it, the demo leaves the plot out.
has_ggplot <- requireNamespace("ggplot2", quietly = TRUE)

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
      tags$h3(whenceOutput("customers", label = "Customers")),
      tags$hr(),
      whenceTableOutput("by_customer", label = "Revenue by customer"),
      if (has_ggplot) {
        tags$div(
          tags$hr(),
          whencePlotOutput("by_channel", label = "Revenue by channel", height = "320px")
        )
      }
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

  # A summary table. Click the first cell of a row.
  output$by_customer <- renderWhenceTable(
    sales |>
      filter(region == input$region, year >= input$from) |>
      filter(channel %in% input$channel) |>
      group_by(customer) |>
      summarise(orders = n(), revenue = sum(revenue)) |>
      mutate(share = round(100 * revenue / sum(revenue), 1)) |>
      arrange(desc(revenue)),
    format = list(revenue = dollars)
  )

  # The same kind of summary, as bars. Click a bar.
  if (has_ggplot) {
    output$by_channel <- renderWhencePlot(
      sales |>
        filter(region == input$region, year >= input$from) |>
        group_by(channel) |>
        summarise(revenue = sum(revenue)),
      plot = function(data) {
        ggplot2::ggplot(data, ggplot2::aes(channel, revenue)) +
          ggplot2::geom_col(fill = "#7356D6") +
          ggplot2::labs(x = NULL, y = "Revenue") +
          ggplot2::theme_minimal(base_size = 14)
      },
      format = list(revenue = dollars)
    )
  }
}

shinyApp(ui, server)
