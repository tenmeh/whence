test_that("input$name is replaced by its value, in quotes", {
  local_dplyr()
  sales <- make_sales()
  input <- list(region = "APAC")

  tr <- whence_trace(
    sales |> filter(region == input$region) |> summarise(n = n())
  )

  expect_match(whence_code(tr), 'filter(region == "APAC")', fixed = TRUE)
  expect_no_match(whence_code(tr), "input", fixed = TRUE)
})

test_that('input[["name"]] is replaced too', {
  local_dplyr()
  sales <- make_sales()
  input <- list(year = 2024L)

  tr <- whence_trace(
    sales |> filter(year >= input[["year"]]) |> summarise(n = n())
  )

  expect_match(whence_code(tr), "filter(year >= 2024L)", fixed = TRUE)
})

test_that("a variable that is not an input is written as it is", {
  local_dplyr()
  sales <- make_sales()
  cutoff <- 2023

  tr <- whence_trace(
    sales |> filter(year >= cutoff) |> summarise(n = n())
  )

  expect_match(whence_code(tr), "filter(year >= cutoff)", fixed = TRUE)
})

test_that("the code parses and gives the same value", {
  local_dplyr()
  sales <- make_sales()
  input <- list(region = "APAC", from = 2023)

  tr <- whence_trace(
    sales |>
      filter(region == input$region) |>
      filter(year >= input[["from"]]) |>
      dplyr::mutate(k = revenue * 2) |>
      summarise(total = sum(k))
  )
  rm(input)
  result <- eval(parse(text = whence_code(tr)))

  expect_identical(result[[1]][[1]], whence_value(tr))
})

test_that("magrittr code with a dot in another position parses and runs", {
  local_dplyr()
  sales <- make_sales()
  first_total <- function(n, data) sum(data$revenue[seq_len(n)])

  tr <- whence_trace(sales %>% filter(region == "APAC") %>% first_total(3, .))
  code <- whence_code(tr)

  expect_identical(eval(parse(text = code)), whence_value(tr))
})

test_that("a date input is written with as.Date()", {
  expect_identical(
    deparse_one(value_literal(as.Date("2026-01-01"))[[1]]),
    'as.Date("2026-01-01")'
  )
  expect_identical(
    deparse_one(value_literal(as.Date(c("2026-01-01", "2026-02-01")))[[1]]),
    'as.Date(c("2026-01-01", "2026-02-01"))'
  )
})

test_that("an input that cannot be evaluated is left as it is", {
  expect_identical(
    substitute_inputs(quote(f(input$missing)), new.env(parent = emptyenv())),
    quote(f(input$missing))
  )
})

test_that("an input of NULL is written as NULL", {
  env <- list2env(list(input = list(a = NULL)))
  expect_identical(substitute_inputs(quote(f(input$a, 1)), env), quote(f(NULL, 1)))
})
