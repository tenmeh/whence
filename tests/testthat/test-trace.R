test_that("two filters give the row drops that were worked out by hand", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(
    sales |>
      filter(region == "APAC") |>
      filter(year >= 2023) |>
      summarise(total = sum(revenue))
  )
  stages <- as.data.frame(tr)

  expect_identical(stages$n, c(20L, 10L, 6L, 1L))
  expect_identical(stages$delta[1:3], c(NA, -10L, -4L))
  expect_identical(stages$label[1:3], c("sales", 'region == "APAC"', "year >= 2023"))
  expect_identical(whence_value(tr), 3900)
})

test_that("a summarise step reports no negative delta", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(sales |> summarise(total = sum(revenue)))
  step <- as.data.frame(tr)[2, ]

  expect_identical(step$kind, "summarise")
  expect_identical(step$delta, NA_integer_)
  expect_identical(step$label, "aggregated to 1 row")
})

test_that("summarize is reported as summarise", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(sales |> summarize(total = sum(revenue)))

  expect_identical(as.data.frame(tr)$kind[2], "summarise")
})

test_that("group_by reports its groups, and summarise reads 'grouped into'", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(
    sales |>
      group_by(region) |>
      summarise(total = sum(revenue)) |>
      filter(region == "EMEA") |>
      select(total)
  )
  stages <- as.data.frame(tr)

  expect_identical(stages$groups[2], 2L)
  expect_identical(stages$label[2], "grouped by region")
  expect_identical(stages$shown[2], TRUE)
  expect_identical(stages$label[3], "grouped into 2 rows")
  expect_identical(stages$delta[3], NA_integer_)
  expect_identical(whence_value(tr), sum(seq(1100, 2000, by = 100)))
})

test_that("count with a grouping column reads 'grouped into'", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(sales |> count(region) |> filter(region == "APAC") |> pull(n))
  stages <- as.data.frame(tr)

  expect_identical(stages$label[2], "grouped into 2 rows")
  expect_identical(stages$delta[2], NA_integer_)
  expect_identical(stages$shown[4], FALSE)
  expect_identical(whence_value(tr), 10L)
})

test_that("a group_by that does not change the groups is hidden", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(
    sales |>
      group_by(region) |>
      group_by(region) |>
      summarise(total = sum(revenue)) |>
      summarise(total = sum(total))
  )

  expect_identical(as.data.frame(tr)$shown[2:3], c(TRUE, FALSE))
})

test_that("arrange, mutate, select and rename have a delta of 0", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(
    sales |>
      arrange(revenue) |>
      mutate(k = revenue / 1000) |>
      select(k) |>
      rename(thousands = k) |>
      summarise(total = sum(thousands))
  )

  expect_identical(as.data.frame(tr)$delta[2:5], rep(0L, 4))
  expect_identical(whence_value(tr), 21)
})

test_that("the value comes from a 1 x 1 tibble", {
  local_dplyr()
  sales <- dplyr::as_tibble(make_sales())

  tr <- whence_trace(sales |> summarise(total = sum(revenue)))

  expect_identical(whence_value(tr), 21000)
})

test_that("a result with one row and two columns names both columns", {
  local_dplyr()
  sales <- make_sales()

  expect_error(
    whence_trace(sales |> summarise(total = sum(revenue), n = n())),
    "`total`, `n`"
  )
})

test_that("a result with many rows gives the row count", {
  local_dplyr()
  sales <- make_sales()

  expect_error(
    whence_trace(sales |> filter(region == "APAC")),
    "returned 10 rows and 4 columns"
  )
})

test_that("input-style values resolve from the captured environment", {
  local_dplyr()
  sales <- make_sales()
  input <- list(region = "EMEA")

  tr <- whence_trace(
    sales |>
      filter(region == input$region) |>
      summarise(n = n())
  )

  expect_identical(whence_value(tr), 10L)
  expect_identical(as.data.frame(tr)$label[2], 'region == "EMEA"')
})

test_that("a magrittr pipeline gives the same waterfall as a native one", {
  local_dplyr()
  sales <- make_sales()

  native <- whence_trace(
    sales |> filter(region == "APAC") |> summarise(total = sum(revenue))
  )
  magrittr <- whence_trace(
    sales %>% filter(region == "APAC") %>% summarise(total = sum(revenue))
  )

  expect_identical(as.data.frame(magrittr), as.data.frame(native))
})

test_that("a step that is not a verb ends the chain, and the value is still given", {
  local_dplyr()
  sales <- make_sales()
  lookup <- data.frame(region = "APAC", manager = "Kim")

  tr <- whence_trace(
    sales |>
      inner_join(lookup, by = "region") |>
      summarise(total = sum(revenue))
  )

  expect_identical(nrow(as.data.frame(tr)), 2L)
  expect_identical(whence_value(tr), 5500)
})

test_that("a pipeline with no steps is not traced, and the value is still given", {
  values <- c(1, 2, 3)

  tr <- whence_trace(sum(values))

  expect_identical(whence_value(tr), 6)
  expect_identical(tr$traced, FALSE)
  expect_match(format(tr), "Pipeline not traced.", fixed = TRUE, all = FALSE)
})

test_that("a lazy table stops with a clear error", {
  lazy <- structure(list(), class = c("tbl_lazy", "tbl"))

  expect_error(whence_trace(lazy), "database-backed pipeline")
})

test_that("the trace prints the waterfall and the code", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(
    sales |> filter(region == "APAC") |> summarise(total = sum(revenue))
  )
  text <- format(tr)

  expect_match(text[1], "5,500", fixed = TRUE)
  expect_match(text, "-10 rows", fixed = TRUE, all = FALSE)
  expect_match(text, "aggregated to 1 row", fixed = TRUE, all = FALSE)
  expect_output(print(tr), "Reproduce")
})

test_that("the data that was given is not changed", {
  local_dplyr()
  sales <- make_sales()
  before <- sales

  tr <- whence_trace(sales |> filter(region == "APAC") |> summarise(n = n()))
  whence_rows(tr)

  expect_identical(sales, before)
})
