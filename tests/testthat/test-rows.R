test_that("a filter chain gives the right source rows", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(
    sales |>
      filter(region == "APAC") |>
      filter(year >= 2023) |>
      summarise(total = sum(revenue))
  )
  rows <- whence_rows(tr)

  expect_identical(rownames(rows), c("3", "4", "5", "8", "9", "10"))
  expect_identical(sum(rows$revenue), whence_value(tr))
})

test_that("the id column is absent from each returned object", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(
    sales |> filter(region == "APAC") |> select(revenue) |> summarise(total = sum(revenue))
  )
  rows <- whence_rows(tr)

  expect_identical(names(rows), names(sales))
  expect_identical(names(tr$source_data), names(sales))
  expect_no_match(whence_code(tr), "whence_id", fixed = TRUE)
  expect_no_match(names(as.data.frame(tr)), "whence", fixed = TRUE)
})

test_that("select and transmute in the chain keep the attribution", {
  local_dplyr()
  sales <- make_sales()

  tr_select <- whence_trace(
    sales |>
      select(region, revenue) |>
      filter(region == "EMEA", revenue > 1500) |>
      summarise(total = sum(revenue))
  )
  tr_transmute <- whence_trace(
    sales |>
      transmute(revenue, big = revenue > 1500) |>
      filter(big) |>
      summarise(total = sum(revenue))
  )

  expect_identical(whence_rows(tr_select)$revenue, c(1600, 1700, 1800, 1900, 2000))
  expect_identical(whence_rows(tr_transmute)$revenue, c(1600, 1700, 1800, 1900, 2000))
})

test_that("mutate with .keep keeps the attribution", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(
    sales |>
      mutate(k = revenue / 100, .keep = "none") |>
      filter(k <= 2) |>
      summarise(total = sum(k))
  )

  expect_identical(nrow(whence_rows(tr)), 2L)
})

test_that("grouped summaries map each group back to its rows", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(
    sales |>
      group_by(region) |>
      summarise(total = sum(revenue)) |>
      filter(region == "EMEA") |>
      pull(total)
  )

  expect_identical(whence_rows(tr)$region, rep("EMEA", 10))
})

test_that("count with a grouping column maps each group back to its rows", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(
    sales |>
      filter(year >= 2024) |>
      count(customer, sort = TRUE) |>
      filter(customer == "Bolt") |>
      pull(n)
  )
  rows <- whence_rows(tr)

  expect_identical(nrow(rows), whence_value(tr))
  expect_identical(unique(rows$customer), "Bolt")
})

test_that("an across() that would change the ids loses attribution, not accuracy", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(
    sales |>
      mutate(across(everything(), as.character)) |>
      mutate(across(everything(), toupper)) |>
      filter(region == "APAC") |>
      summarise(n = n())
  )

  expect_warning(rows <- whence_rows(tr), class = "whence_rows_unavailable")
  expect_null(rows)
})

test_that("distinct gives NULL with the verb named, and the waterfall is complete", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(
    sales |>
      filter(region == "APAC") |>
      distinct(customer) |>
      summarise(n = n())
  )

  expect_warning(
    rows <- whence_rows(tr),
    "Row attribution unavailable past distinct()",
    fixed = TRUE,
    class = "whence_rows_unavailable"
  )
  expect_null(rows)
  condition <- tryCatch(whence_rows(tr), warning = function(w) w)
  expect_identical(condition$verb, "distinct")
  expect_identical(as.data.frame(tr)$n, c(20L, 10L, 4L, 1L))
})

test_that("pivot steps are counted, and attribution stops there", {
  local_dplyr()
  skip_if_not_installed("tidyr")
  sales <- make_sales()

  tr <- whence_trace(
    sales |>
      filter(region == "APAC") |>
      tidyr::pivot_longer(c(year, revenue)) |>
      summarise(n = n())
  )

  expect_identical(as.data.frame(tr)$delta[3], 10L)
  expect_warning(whence_rows(tr), "pivot_longer", class = "whence_rows_unavailable")
})

test_that("whence_trace() does not run the identity pass", {
  local_dplyr()
  calls <- 0
  get_sales <- function() {
    calls <<- calls + 1
    make_sales()
  }

  tr <- whence_trace(get_sales() |> filter(region == "APAC") |> summarise(n = n()))
  expect_identical(calls, 1)

  marker <- 0
  tr <- whence_trace(
    make_sales() |>
      filter({
        marker <<- marker + 1
        region == "APAC"
      }) |>
      summarise(n = n())
  )
  expect_identical(marker, 1)

  whence_rows(tr)
  expect_identical(marker, 2)
})

test_that("slice_sample is replayed with the same draws", {
  local_dplyr()
  sales <- make_sales()
  set.seed(42)

  tr <- whence_trace(
    sales |> slice_sample(n = 5) |> summarise(total = sum(revenue))
  )
  state <- .Random.seed
  rows <- whence_rows(tr)

  expect_identical(sum(rows$revenue), whence_value(tr))
  expect_identical(.Random.seed, state)
})

test_that("n limits the number of rows", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(sales |> filter(region == "APAC") |> summarise(n = n()))

  expect_identical(nrow(whence_rows(tr, n = 3)), 3L)
  expect_error(whence_rows(tr, n = -1), "`n` must be")
  expect_error(whence_rows(list()), "must be a `whence_trace`")
})

test_that("rows are unavailable for a pipeline that was not traced", {
  tr <- whence_trace(sum(1:3))

  expect_warning(rows <- whence_rows(tr), "not traced", class = "whence_rows_unavailable")
  expect_null(rows)
  expect_match(rows_unavailable_reason(tr), "not traced")
})
test_that("hidden id predicate drift loses attribution with a named warning", {
  local_dplyr()
  d <- data.frame(x = c("yes", "no"))
  tr <- whence_trace(d |> filter(if_all(where(is.character), ~ .x == "yes")) |> summarise(n = n()))
  expect_identical(whence_value(tr), 1L)
  expect_warning(rows <- whence_rows(tr), "hidden id changed the result of filter\\(\\)")
  expect_null(rows)
})

test_that("count keys distinguish missing values and literal NA", {
  local_dplyr()
  d <- data.frame(g = c(NA_character_, "NA"))
  tr <- whence_trace(d |> count(g) |> filter(is.na(g)) |> pull(n))
  expect_identical(whence_value(tr), 1L)
  expect_identical(whence_rows(tr), d[1, , drop = FALSE])
})

test_that("count keys distinguish embedded separators", {
  local_dplyr()
  d <- data.frame(a = c("a\037b", "a"), b = c("c", "b\037c"))
  tr <- whence_trace(d |> count(a, b) |> filter(a == "a") |> pull(n))
  expect_identical(whence_rows(tr), d[2, , drop = FALSE])
})

test_that("row replay uses frozen input values", {
  local_dplyr()
  d <- data.frame(x = 1:3)
  input <- list(k = 1)
  tr <- whence_trace(d |> filter(x == input$k) |> summarise(z = sum(x)))
  input$k <- 3
  expect_identical(whence_rows(tr), d[1, , drop = FALSE])
})

test_that("row replay detects changed counts from live variables", {
  local_dplyr()
  d <- data.frame(x = 1:3)
  k <- 1
  tr <- whence_trace(d |> filter(x > k) |> summarise(z = sum(x)))
  k <- 3
  expect_warning(rows <- whence_rows(tr), class = "whence_rows_unavailable")
  expect_null(rows)
})

test_that("empty sources and empty aggregates keep zero ids", {
  local_dplyr()
  d <- data.frame(x = integer())
  expect_identical(whence_rows(whence_trace(d |> summarise(n = n()))), d)
  expect_identical(whence_rows(whence_trace(d |> count(x) |> summarise(n = n()))), d)
})

test_that("step gives the rows that one step removed", {
  local_dplyr()
  sales <- make_sales()

  tr <- whence_trace(
    sales |>
      filter(region == "APAC") |>
      filter(year >= 2023) |>
      summarise(total = sum(revenue))
  )

  expect_identical(nrow(whence_rows(tr, step = 1)), 10L)
  expect_identical(unique(whence_rows(tr, step = 1)$region), "EMEA")
  expect_identical(rownames(whence_rows(tr, step = 2)), c("1", "2", "6", "7"))
  expect_identical(nrow(whence_rows(tr, step = 3)), 0L)
  expect_identical(nrow(whence_rows(tr, n = 2, step = 1)), 2L)
})

test_that("step must be the number of a step", {
  local_dplyr()
  sales <- make_sales()
  tr <- whence_trace(sales |> filter(region == "APAC") |> summarise(n = n()))

  expect_error(whence_rows(tr, step = 0), "a whole number from 1 to 2")
  expect_error(whence_rows(tr, step = 3), "a whole number from 1 to 2")
  expect_error(whence_rows(tr, step = "1"), "a whole number from 1 to 2")
  expect_error(whence_rows(whence_trace(sum(1:3)), step = 1), "this pipeline has no steps")
})

test_that("a step before distinct() can still show its removed rows", {
  local_dplyr()
  sales <- make_sales()
  tr <- whence_trace(
    sales |>
      filter(region == "APAC") |>
      distinct(customer) |>
      summarise(n = n())
  )

  expect_identical(nrow(whence_rows(tr, step = 1)), 10L)
  expect_warning(rows <- whence_rows(tr, step = 2), "past distinct()", fixed = TRUE)
  expect_null(rows)
})

test_that("after a summary, a step that drops groups removes their source rows", {
  local_dplyr()
  sales <- make_sales()
  tr <- whence_trace(
    sales |>
      group_by(region) |>
      summarise(total = sum(revenue)) |>
      filter(total > 10000) |>
      pull(total)
  )

  removed <- whence_rows(tr, step = 3)

  expect_identical(unique(removed$region), "APAC")
  expect_identical(nrow(removed), 10L)
})
