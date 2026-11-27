test_that("the grid gives each row the counts of its own group", {
  local_dplyr()
  sales <- make_sales()
  tab <- attach_replay(whence_table(
    sales |>
      filter(year >= 2022) |>
      group_by(region) |>
      summarise(n = n())
  ))
  grid <- hover_grid(tab)
  expect_length(grid$counts, nrow(tab$value))
  for (i in seq_len(nrow(tab$value))) {
    g <- whence_group(tab, row = i)
    expect_identical(grid$counts[[i]], g$stages$n[g$stages$shown])
  }
})

test_that("a replay that cannot run drops the grid, not the render", {
  local_dplyr()
  tab <- attach_replay(whence_table(make_sales() |> distinct(r = tolower(region)) |> count(r)))
  grid <- hover_grid(tab)
  expect_null(tab$replay)
  expect_null(grid$counts)
  expect_match(grid$reason, "Counts for this row are not known")
  expect_match(grid$reason, "distinct()", fixed = TRUE)
})

test_that("a click after render reuses the stored replay", {
  local_dplyr()
  calls <- 0L
  real <- replay_trace
  local_mocked_bindings(replay_trace = function(...) {
    calls <<- calls + 1L
    real(...)
  })
  tab <- attach_replay(whence_table(make_sales() |> count(region)))
  whence_group(tab, row = 1)
  expect_identical(calls, 1L)
})

test_that("the option 'none' skips the replay", {
  local_dplyr()
  withr::local_options(whence.hover = "none")
  calls <- 0L
  local_mocked_bindings(replay_trace = function(...) {
    calls <<- calls + 1L
  })
  expect_null(attach_replay(whence_table(make_sales() |> count(region)))$replay)
  expect_identical(calls, 0L)
})

test_that("a table of more than 500 rows has a grid of 500", {
  local_dplyr()
  big <- data.frame(id = seq_len(800), x = 1)
  tab <- attach_replay(whence_table(big |> group_by(id) |> summarise(x = sum(x))))
  expect_length(table_payload(tab)$hover$counts, 500L)
})

test_that("an unexpected replay error leaves the rendered table available", {
  local_dplyr()
  local_mocked_bindings(replay_trace = function(...) stop("replay error", call. = FALSE))
  sales <- make_sales()
  server <- function(input, output, session) {
    output$tab <- renderWhenceTable(sales |> count(region))
  }

  shiny::testServer(server, {
    expect_identical(output$tab$shown, 2L)
    expect_null(output$tab$hover$counts)
    expect_match(output$tab$hover$reason, "Counts for this row are not known: the replay failed.", fixed = TRUE)
  })
})
