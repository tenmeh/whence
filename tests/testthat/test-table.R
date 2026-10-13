by_region <- function(sales) {
  whence_table(
    sales |>
      filter(year >= 2023) |>
      group_by(region) |>
      summarise(total = sum(revenue)) |>
      mutate(share = total / sum(total))
  )
}

test_that("a table trace keeps the table and the group columns", {
  local_dplyr()
  sales <- make_sales()

  tab <- by_region(sales)

  expect_s3_class(tab, c("whence_table", "whence_trace"))
  expect_identical(tab$keys, "region")
  expect_identical(nrow(whence_value(tab)), 2L)
  expect_identical(as.data.frame(tab)$n, c(20L, 12L, 12L, 2L, 2L))
  expect_match(format(tab)[1], "2 rows, one for each region", fixed = TRUE)
})

test_that("the group columns come from count() and from summarise(.by = )", {
  local_dplyr()
  sales <- make_sales()

  expect_identical(whence_table(sales |> count(region, customer))$keys, c("region", "customer"))
  expect_identical(
    whence_table(sales |> mutate(big = revenue > 1000) |> summarise(n = n(), .by = big))$keys,
    "big"
  )
  expect_null(whence_table(sales |> group_by(region) |> summarise(t = sum(revenue)) |> select(t))$keys)
})

test_that("a table pipeline must give a data frame", {
  local_dplyr()
  sales <- make_sales()

  expect_error(whence_table(sales |> pull(revenue)), "whence can explain a table")
})

test_that("a group has the counts of its own rows at each step", {
  local_dplyr()
  sales <- make_sales()

  apac <- whence_group(by_region(sales), 1)
  stages <- as.data.frame(apac)

  expect_s3_class(apac, c("whence_group", "whence_trace"))
  expect_identical(apac$label, "region = APAC")
  expect_identical(stages$n, c(10L, 6L, 6L, 1L, 1L))
  expect_identical(stages$delta[2], -4L)
  expect_equal(stages$percent[2], -40)
  expect_identical(stages$label[4], "summarised into this row")
})

test_that("the code of a group gives its row, also when a later step compares groups", {
  local_dplyr()
  sales <- make_sales()
  tab <- by_region(sales)

  apac <- whence_group(tab, 1)
  row <- eval(parse(text = whence_code(apac)))

  expect_match(whence_code(apac), 'filter(region == "APAC")', fixed = TRUE)
  expect_equal(row$share, whence_value(tab)$share[1])
  expect_equal(whence_value(apac)$total, 3900)
})

test_that("the rows of a group, and the rows that a step removed from it", {
  local_dplyr()
  sales <- make_sales()
  apac <- whence_group(by_region(sales), 1)

  rows <- whence_rows(apac)
  removed <- whence_rows(apac, step = 1)

  expect_identical(rownames(rows), c("3", "4", "5", "8", "9", "10"))
  expect_identical(rownames(removed), c("1", "2", "6", "7"))
  expect_identical(unique(removed$region), "APAC")
})

test_that("a group whose key is made by a step has no count before that step", {
  local_dplyr()
  sales <- make_sales()

  big <- whence_group(whence_table(sales |> mutate(big = revenue > 1000) |> count(big)), 2)

  expect_identical(as.data.frame(big)$n, c(NA, 10L, 1L))
  expect_identical(nrow(whence_rows(big)), 10L)
})

test_that("NA and the string 'NA' are different groups", {
  local_dplyr()
  d <- data.frame(g = c(NA, "NA", "NA"), x = 1:3)

  tab <- whence_table(d |> count(g))
  na_row <- which(is.na(whence_value(tab)$g))

  expect_identical(nrow(whence_rows(whence_group(tab, na_row))), 1L)
  expect_match(whence_code(whence_group(tab, na_row)), "filter(is.na(g))", fixed = TRUE)
})

test_that("past distinct() the counts of a group are not known, and say so", {
  local_dplyr()
  sales <- make_sales()

  g <- whence_group(whence_table(sales |> distinct(region, customer) |> count(region)), 1)

  expect_identical(as.data.frame(g)$n[2:3], c(NA_integer_, NA_integer_))
  expect_match(g$reason, "Counts for this group are not known past distinct()", fixed = TRUE)
  expect_warning(whence_rows(g), class = "whence_rows_unavailable")
})

test_that("whence_group() checks its arguments", {
  local_dplyr()
  sales <- make_sales()
  tab <- by_region(sales)

  expect_error(whence_group(whence_trace(sales |> summarise(n = n())), 1), "must be a `whence_table`")
  expect_error(whence_group(tab, 3), "`row` must be a whole number from 1 to 2")
  expect_error(whence_group(tab, 1.5), "`row` must be")
  no_keys <- whence_table(sales |> group_by(region) |> summarise(t = sum(revenue)) |> select(t))
  expect_error(whence_group(no_keys, 1), "cannot be explained one by one")
})

test_that("a group prints its label and its waterfall", {
  local_dplyr()
  sales <- make_sales()

  text <- format(whence_group(by_region(sales), 2))

  expect_identical(text[1], "<whence_group> region = EMEA")
  expect_match(text, "-4 rows (40%)", fixed = TRUE, all = FALSE)
})
