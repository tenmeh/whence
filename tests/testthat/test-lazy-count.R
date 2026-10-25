local_lazy_backend <- function(backend, data = make_sales(), envir = parent.frame()) {
  if (backend == "dbplyr") {
    skip_if_not_installed("dbplyr")
    skip_if_not_installed("RSQLite")
    skip_if_not_installed("DBI")
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    withr::defer(DBI::dbDisconnect(con), envir = envir)
    return(dplyr::copy_to(con, data, "sales", temporary = TRUE))
  }
  if (backend %in% c("arrow_table", "arrow_query")) {
    skip_if_not_installed("arrow")
    tab <- arrow::Table$create(data)
    if (backend == "arrow_query") {
      tab <- dplyr::filter(tab, revenue >= 0)
    }
    return(tab)
  }
  if (backend == "dtplyr") {
    skip_if_not_installed("dtplyr")
    return(dtplyr::lazy_dt(data))
  }
  skip_if_not_installed("duckplyr")
  duckplyr::as_duckdb_tibble(data, prudence = "stingy")
}

for (backend in c("dbplyr", "arrow_table", "arrow_query", "dtplyr", "duckplyr")) {
  test_that(paste("lazy counts, values and tables match local data for", backend), {
    local_dplyr()
    sales <- make_sales()
    tab <- local_lazy_backend(backend, sales)
    expect_identical(lazy_row_count(tab), 20L)
    # A stingy duckplyr table refuses group_by() in any code.
    if (backend != "duckplyr") {
      expect_identical(lazy_row_count(group_by(tab, region)), 20L)
    }
    expect_identical(lazy_row_count(filter(tab, revenue < 0)), 0L)
    remote <- whence_trace(
      tab |> filter(year >= 2023) |> summarise(total = sum(revenue)),
      lazy = "count"
    )
    local <- whence_trace(sales |> filter(year >= 2023) |> summarise(total = sum(revenue)))
    expect_identical(remote$stages$n, local$stages$n)
    expect_identical(remote$stages$delta, local$stages$delta)
    expect_equal(remote$value, local$value)
    expect_identical(remote$traced, TRUE)
    expect_null(remote$reason)
    expect_match(remote$code, "dplyr::collect()", fixed = TRUE)
    expect_equal(extract_scalar(eval(parse(text = remote$code))), remote$value)

    remote_table <- whence_table(
      tab |>
        filter(year >= 2023) |>
        summarise(total = sum(revenue), .by = region) |>
        arrange(region),
      lazy = "count"
    )
    local_table <- whence_table(
      sales |>
        filter(year >= 2023) |>
        summarise(total = sum(revenue), .by = region) |>
        arrange(region)
    )
    expect_identical(remote_table$stages$n, local_table$stages$n)
    expect_identical(remote_table$stages$delta, local_table$stages$delta)
    expect_identical(remote_table$keys, "region")
    expect_equal(as.data.frame(remote_table$value), as.data.frame(local_table$value))
    expect_warning(rows <- whence_rows(remote), "the data is a lazy table", fixed = TRUE)
    expect_null(rows)
    expect_identical(panel_payload(remote)$rowsAvailable, FALSE)
    expect_identical(removable_steps(remote), integer())
    group <- whence_group(remote_table, region = "APAC")
    expect_warning(rows <- whence_rows(group), "the data is a lazy table", fixed = TRUE)
    expect_null(rows)
  })
}

test_that("lazy modes validate strictly and use the option default", {
  local_dplyr()
  for (bad in list("cou", "STOP", NA_character_, c("stop", "count"), NULL, 1, TRUE)) {
    expect_error(whence_trace(stop("must not run"), lazy = bad), '`lazy` must be "stop" or "count"', fixed = TRUE)
    expect_error(whence_table(stop("must not run"), lazy = bad), '`lazy` must be "stop" or "count"', fixed = TRUE)
  }
  expect_error(whence_trace(1, lazy = "cou"), '`lazy` must be "stop" or "count", not "cou".', fixed = TRUE)
  for (render in list(renderWhence, renderWhenceTable, renderWhencePlot)) {
    expect_error(render(1, lazy = "cou"), 'not "cou"', fixed = TRUE)
  }
  tab <- local_lazy_backend("dbplyr")
  withr::local_options(whence.lazy = "count")
  expect_equal(whence_trace(tab |> summarise(n = n()))$value, 20)
  expect_equal(nrow(whence_table(tab |> count(region))$value), 2)
  expect_error(whence_trace(tab |> summarise(n = n()), lazy = "stop"), "tbl_lazy", fixed = TRUE)
})

test_that("lazy joins count removals and repeated matches with one extra count", {
  local_dplyr()
  tab <- local_lazy_backend("dbplyr")
  y <- filter(tab, revenue %in% c(100, 200, 1100)) |> select(region, manager = revenue)
  calls <- 0L
  original_count <- lazy_row_count
  local_mocked_bindings(lazy_row_count = function(x) {
    calls <<- calls + 1L
    original_count(x)
  })
  tr <- whence_trace(
    tab |> filter(year >= 2023) |> inner_join(y, by = "region") |> summarise(n = n()),
    lazy = "count"
  )
  expect_identical(calls, 5L)
  expect_identical(tr$stages$n, c(20L, 12L, 18L, 1L))
  expect_identical(tr$stages$delta[[3]], 0L)
  expect_identical(tr$stages$added[[3]], 6L)
  y <- filter(y, region == "APAC")
  tr <- whence_trace(tab |> inner_join(y, by = "region") |> summarise(n = n()), lazy = "count")
  expect_identical(tr$stages$delta[[2]], -10L)
  expect_identical(tr$stages$added[[2]], 10L)
})

test_that("lazy counts are cached across labels, groups and percentages", {
  local_dplyr()
  tab <- local_lazy_backend("dbplyr")
  calls <- 0L
  original_count <- lazy_row_count
  local_mocked_bindings(lazy_row_count = function(x) {
    calls <<- calls + 1L
    original_count(x)
  })
  tr <- whence_table(tab |> group_by(region) |> summarise(n = n()), lazy = "count")
  expect_identical(calls, 3L)
  expect_identical(tr$stages$n, c(20L, 20L, 2L))
  expect_identical(tr$keys, "region")
  expect_identical(tr$stages$shown, c(TRUE, TRUE, TRUE))
  expect_identical(tr$stages$groups, rep(NA_integer_, 3))
})

test_that("row attribution starts at collect and preserves original step numbers", {
  local_dplyr()
  tab <- local_lazy_backend("dbplyr")
  tr <- whence_trace(
    tab |> filter(year >= 2023) |> collect() |> filter(region == "APAC") |> summarise(n = n()),
    lazy = "count"
  )
  expect_identical(tr$stages$n, c(20L, 12L, 12L, 6L, 1L))
  expect_identical(whence_rows(tr)$revenue, c(300, 400, 500, 800, 900, 1000))
  expect_identical(whence_rows(tr, step = 3)$revenue, c(1300, 1400, 1500, 1800, 1900, 2000))
  for (step in 1:2) {
    expect_warning(
      rows <- whence_rows(tr, step = step),
      "steps before collect() run in the database", fixed = TRUE
    )
    expect_null(rows)
  }
  expect_identical(removable_steps(tr), 3L)
  expect_null(rows_unavailable_reason(tr))
  expect_identical(panel_payload(tr)$rowsAvailable, TRUE)
  expect_identical(panel_payload(tr)$stages[[2]]$removable, FALSE)
  expect_identical(panel_payload(tr)$stages[[3]]$removable, TRUE)

  table <- whence_table(
    tab |>
      filter(year >= 2023) |>
      collect() |>
      filter(revenue > 300) |>
      group_by(region) |>
      summarise(n = n()),
    lazy = "count"
  )
  group <- whence_group(table, region = "APAC")
  expect_identical(whence_rows(group)$revenue, c(400, 500, 800, 900, 1000))
  expect_identical(whence_rows(group, step = 3)$revenue, 300)
  expect_identical(group$stages$n[1:4], c(NA_integer_, NA_integer_, 6L, 5L))
})

test_that("lazy counts support .by keys and counts without exposing internal names", {
  local_dplyr()
  tab <- local_lazy_backend("dbplyr")
  by <- whence_table(tab |> summarise(n = n(), .by = region), lazy = "count")
  counted <- whence_table(tab |> count(region), lazy = "count")
  expect_identical(by$keys, "region")
  expect_identical(counted$keys, "region")
  expect_identical(by$stages$n, c(20L, 2L))
})

test_that("Shiny render functions pass lazy counting through", {
  local_dplyr()
  tab <- local_lazy_backend("dbplyr")
  server <- function(input, output, session) {
    output$total <- renderWhence(
      tab |> filter(region == input$region) |> summarise(n = n()), lazy = "count"
    )
    output$table <- renderWhenceTable(tab |> count(region), lazy = "count")
  }
  shiny::testServer(server, {
    session$setInputs(region = "APAC")
    session$flushReact()
    expect_identical(output$total$value, "10")
    expect_identical(vapply(output$total$stages, `[[`, 1L, "n"), c(20L, 10L, 1L))
    expect_identical(output$total$rowsAvailable, FALSE)
    expect_identical(output$table$kind, "table")
    expect_identical(output$table$total, 2L)
  })
})
