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
  expect_equal(whence_value(tr), 21)
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
  expect_identical(tr$reason, "Not traced: the pipeline has no step that whence knows.")
  expect_match(format(tr), tr$reason, fixed = TRUE, all = FALSE)
})

test_that("an untraced pipeline names the call where the chain stopped", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(sales |> filter(year > 2022) |> nrow())

  expect_identical(tr$traced, FALSE)
  expect_identical(tr$reason, "Not traced: the chain stops at nrow(), which whence does not know.")
  expect_match(format(tr), "the chain stops at nrow()", fixed = TRUE, all = FALSE)
})

test_that("a join in the chain is named, and the steps after it are traced", {
  local_dplyr()
  sales <- make_sales()
  managers <- data.frame(region = c("APAC", "EMEA"), manager = c("Kim", "Lou"))

  tr <- whence_trace(
    sales |>
      filter(year > 2022) |>
      dplyr::left_join(managers, by = "region") |>
      summarise(n = n())
  )

  expect_identical(tr$traced, TRUE)
  expect_identical(
    tr$reason,
    "Steps before left_join() are not shown, because whence does not know left_join()."
  )
  expect_match(format(tr), tr$reason, fixed = TRUE, all = FALSE)
})

test_that("a fully traced pipeline has no reason", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(sales |> filter(year > 2022) |> summarise(n = n()))

  expect_null(tr$reason)
})

test_that("a source that is not a data frame is the reason", {
  tr <- whence_trace(c(3, 1, 2) |> head(1))

  expect_identical(tr$reason, "Not traced: the source is not a data frame.")
})

test_that("the help page lists every known verb", {
  path <- test_path("..", "..", "man", "whence_trace.Rd")
  skip_if_not(file.exists(path), "The package source is not available.")
  rd <- paste(readLines(path), collapse = " ")

  documented <- vapply(
    whence_verbs,
    function(v) grepl(paste0("\\code{", v, "()}"), rd, fixed = TRUE),
    logical(1)
  )
  expect_identical(names(documented)[!documented], character(0))
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
test_that("single expression blocks are traced", {
  local_dplyr()
  d <- data.frame(x = 1:4)
  tr <- whence_trace({
    d |> filter(x > 2) |> summarise(n = n())
  })
  expect_identical(tr$traced, TRUE)
  expect_identical(tr$stages$n, c(4L, 2L, 1L))
})

test_that("multiple expression blocks trace with local bindings", {
  local_dplyr()
  d <- data.frame(x = 1:4)
  k <- 99
  tr <- whence_trace({
    k <- 2
    d |> filter(x > k) |> summarise(n = n())
  })
  expect_identical(tr$traced, TRUE)
  expect_identical(tr$stages$n, c(4L, 2L, 1L))
  expect_identical(whence_value(tr), 2L)
  expect_identical(k, 99)
  expect_identical(whence_rows(tr), d[3:4, , drop = FALSE])
})

test_that("regrouping with the same count is shown", {
  local_dplyr()
  d <- data.frame(a = c(1, 1, 2, 2), b = c(1, 2, 1, 2))
  tr <- whence_trace(d |> group_by(a) |> group_by(b) |> ungroup() |> summarise(n = n()))
  expect_identical(tr$stages$shown[3], TRUE)
  expect_identical(tr$stages$label[3], "grouped by b")
})

test_that("each step has its number and the percentage of rows it removed", {
  local_dplyr()
  sales <- make_sales()

  stages <- as.data.frame(whence_trace(
    sales |> filter(region == "APAC") |> filter(year >= 2023) |> summarise(n = n())
  ))

  expect_identical(stages$step, 0:3)
  expect_equal(stages$percent, c(NA, -50, -40, NA))
})

test_that("percentages are rounded, a small one is <1%, and 100% means all", {
  expect_identical(
    format_percent(c(-50, -0.4, NA, -99.6, 12.5, -100)),
    c("50%", "<1%", "", "99%", "12%", "100%")
  )
})
