test_that("whence_opened() gives the open row of a table, and NULL when closed", {
  local_dplyr()
  sales <- make_sales()
  server <- function(input, output, session) {
    output$tab <- renderWhenceTable(sales |> count(region))
    output$total <- renderWhence(sales |> summarise(n = n()))
    tab_opened <- whence_opened("tab")
    total_opened <- whence_opened("total")
  }

  shiny::testServer(server, {
    expect_identical(output$tab$kind, "table")
    expect_identical(output$total$value, "20")
    expect_null(tab_opened())

    session$setInputs(.whence_opened = list(id = "tab", row = 2, open = TRUE))
    expect_identical(tab_opened()$row, 2L)
    expect_identical(tab_opened()$data$region, "EMEA")
    expect_null(total_opened())

    session$setInputs(.whence_opened = list(id = "total", open = TRUE))
    expect_null(tab_opened())
    expect_identical(total_opened()$data, 20L)
    expect_null(total_opened()$row)

    session$setInputs(.whence_opened = list(id = "total", open = FALSE))
    expect_null(total_opened())
  })
})

test_that("a row from the browser that is not in the table gives no row", {
  local_dplyr()
  sales <- make_sales()
  server <- function(input, output, session) {
    output$tab <- renderWhenceTable(sales |> count(region))
    opened <- whence_opened("tab")
  }

  shiny::testServer(server, {
    expect_identical(output$tab$kind, "table")
    for (row in list(9, -1, "1", 1.5)) {
      session$setInputs(.whence_opened = list(id = "tab", row = row, open = TRUE))
      expect_null(opened()$row)
    }
  })
})

test_that("in a module, whence_opened() takes the id without the namespace", {
  local_dplyr()
  sales <- make_sales()
  module <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      output$tab <- renderWhenceTable(sales |> count(region))
      whence_opened("tab")
    })
  }
  server <- function(input, output, session) {
    opened <- module("mod")
    output$state <- shiny::renderText(if (is.null(opened())) "closed" else opened()$data$region)
  }

  shiny::testServer(server, {
    expect_identical(output$state, "closed")
    session$setInputs(.whence_opened = list(id = "mod-tab", row = 1, open = TRUE))
    expect_identical(output$state, "APAC")
  })
})

test_that("whence_opened() checks its arguments", {
  expect_error(whence_opened(1, session = list()), "`outputId` must be a single string")
  expect_error(whence_opened("x", session = NULL), "must be called in a 'shiny' server function")
})
