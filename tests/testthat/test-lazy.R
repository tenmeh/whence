test_that("materialising steps are hidden and preserve source rows", {
  local_dplyr()
  sales <- make_sales()
  tr <- whence_trace(
    sales |>
      collect() |>
      dplyr::as_tibble() |>
      as.data.frame() |>
      filter(year >= 2023) |>
      summarise(n = n())
  )
  expect_identical(tr$stages$shown[2:4], rep(FALSE, 3))
  expect_identical(tr$stages$delta[2:4], rep(NA_integer_, 3))
  expect_identical(tr$stages$n[2:4], rep(nrow(sales), 3))
  expect_identical(whence_rows(tr), sales[sales$year >= 2023, ])
  expect_identical(whence_rows(tr, step = 4), sales[sales$year < 2023, ])
})

test_that("all materialising verbs decompose with or without namespaces", {
  for (verb in materialising_verbs) {
    for (fn in list(as.symbol(verb), call("::", quote(pkg), as.symbol(verb)))) {
      parts <- decompose_pipeline(as.call(list(fn, quote(x))))
      expect_identical(parts$source, quote(x))
      expect_identical(parts$stages[[1]]$verb, verb)
    }
  }
})

test_that("lazy detection uses classes without counting rows", {
  for (kind in names(lazy_table_kinds)) {
    x <- structure(list(), class = kind)
    expect_identical(is_lazy_table(x), TRUE)
    expect_error(stop_lazy_table(x), "Call `collect()` before", fixed = TRUE)
    expect_error(whence_trace(x), kind, fixed = TRUE)
  }
  expect_identical(is_lazy_table(data.frame()), FALSE)
})

test_that("the lazy re-split names the backend for every lazy class", {
  locations <- c(
    tbl_lazy = "in the database",
    arrow_dplyr_query = "in 'arrow'",
    ArrowTabular = "in 'arrow'",
    Dataset = "in 'arrow'",
    dtplyr_step = "in 'dtplyr'",
    prudent_duckplyr_df = "in 'duckdb'"
  )
  collect <- function(x) data.frame(n = 1L)
  for (kind in names(locations)) {
    x <- structure(list(), class = kind)
    tr <- whence_trace(collect(x))
    expect_identical(tr$reason, paste0(
      "Steps before collect() run ", locations[[kind]], ", so they are not counted."
    ))
  }
})

test_that("the lazy re-split binds dots outside the data slot", {
  local_dplyr()
  x <- structure(list(n = 7L), class = "dtplyr_step")
  collect <- function(x, n) data.frame(n = n)
  tr <- whence_trace(x %>% collect(., n = .$n))
  expect_identical(tr$value, 7L)
  expect_identical(tr$source[[1]][[1]], quote(`function`))
  expect_identical(tr$source[[2]], quote(x))
  expect_identical(extract_scalar(eval(tr$source, environment())), 7L)
})

test_that("dbplyr errors before counting and falls back at the last materialiser", {
  local_dplyr()
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sales <- make_sales()
  db <- copy_to(con, sales, "sales", temporary = TRUE)
  expect_error(
    whence_trace(db |> filter(year >= 2023) |> summarise(n = n())),
    "a `tbl_lazy` from 'dbplyr'", fixed = TRUE
  )
  tr <- whence_trace(
    db |>
      filter(year >= 2023) |>
      compute() |>
      collect() |>
      filter(region == "APAC") |>
      summarise(n = n())
  )
  expect_identical(tr$stages$verb, c(NA_character_, "filter", "summarise"))
  expect_identical(tr$reason, paste0(
    "Steps before collect() run in the database, so they are not counted."
  ))
  expect_identical(safe_call_name(tr$source), "collect")
  expect_equal(
    as.data.frame(whence_rows(tr)),
    sales[sales$year >= 2023 & sales$region == "APAC", ],
    ignore_attr = TRUE
  )
  expect_equal(whence_value(tr), 6)
  expect_error(whence_trace(db |> compute() |> summarise(n = n())), "tbl_lazy", fixed = TRUE)
  nested <- whence_trace(summarise(dplyr::collect(filter(db, year >= 2023)), n = n()))
  piped <- whence_trace(db %>% filter(year >= 2023) %>% collect() %>% summarise(n = n()))
  expect_equal(whence_value(nested), 12)
  expect_equal(whence_value(piped), 12)
  expect_equal(nrow(whence_rows(piped)), 12)
  dot <- whence_trace(db %>% filter(year >= 2023) %>% collect(.) %>% summarise(n = n()))
  expect_equal(whence_value(dot), 12)
  expect_identical(dot$source, quote(collect(filter(db, year >= 2023))))
  expect_equal(extract_scalar(eval(parse(text = whence_code(dot)), environment())), 12)
})

test_that("arrow Tables and queries give clear errors and collect works", {
  local_dplyr()
  skip_if_not_installed("arrow")
  tab <- arrow::Table$create(make_sales())
  expect_error(whence_trace(tab), "arrow Table", fixed = TRUE)
  query <- filter(tab, year >= 2023)
  expect_error(whence_trace(query |> summarise(n = n())), "arrow_dplyr_query", fixed = TRUE)
  tr <- whence_trace(query |> collect() |> filter(region == "APAC") |> summarise(n = n()))
  expect_equal(whence_value(tr), 6)
  expect_equal(nrow(whence_rows(tr)), 6)
  expect_identical(tr$reason, "Steps before collect() run in 'arrow', so they are not counted.")
})

test_that("dtplyr errors before counting and as_tibble materialises", {
  local_dplyr()
  skip_if_not_installed("dtplyr")
  tab <- dtplyr::lazy_dt(make_sales())
  expect_error(whence_trace(tab |> summarise(n = n())), "dtplyr_step", fixed = TRUE)
  tr <- whence_trace(tab |> dplyr::as_tibble() |> filter(year >= 2023) |> summarise(n = n()))
  expect_equal(whence_value(tr), 12)
  expect_equal(nrow(whence_rows(tr)), 12)
  expect_identical(tr$reason, "Steps before as_tibble() run in 'dtplyr', so they are not counted.")
})

test_that("arrow Datasets are detected before their rows are counted", {
  local_dplyr()
  skip_if_not_installed("arrow")
  path <- withr::local_tempdir()
  arrow::write_dataset(make_sales(), path)
  dataset <- arrow::open_dataset(path)
  expect_error(whence_trace(dataset), "a `Dataset` from 'arrow'", fixed = TRUE)
})

test_that("prudent duckplyr errors clearly while default tables are traced", {
  local_dplyr()
  skip_if_not_installed("duckplyr")
  sales <- make_sales()
  stingy <- duckplyr::as_duckdb_tibble(sales, prudence = "stingy")
  expect_error(whence_trace(stingy |> summarise(n = n())), "prudent_duckplyr_df", fixed = TRUE)
  thrifty <- duckplyr::as_duckdb_tibble(sales, prudence = "thrifty")
  expect_error(whence_trace(thrifty), "prudent_duckplyr_df", fixed = TRUE)
  ordinary <- duckplyr::as_duckdb_tibble(sales)
  tr <- whence_trace(ordinary |> filter(year >= 2023) |> summarise(n = n()))
  expect_identical(tr$traced, TRUE)
  expect_equal(tr$stages$n, c(20, 12, 1))
  expect_equal(whence_value(tr), 12)
})

test_that("a stage result is checked for laziness before counting", {
  local_dplyr()
  sales <- make_sales()
  mutate <- function(.data, ...) structure(list(), class = "dtplyr_step")
  expect_error(whence_trace(sales |> mutate()), "dtplyr_step", fixed = TRUE)
})

test_that("materialising replay still checks the row count", {
  local_dplyr()
  sales <- make_sales()
  collect <- function(x) {
    if (".whence_id" %in% names(x)) x[-1, ] else x
  }
  tr <- whence_trace(sales |> collect() |> summarise(n = n()))
  expect_warning(rows <- whence_rows(tr), "the hidden id changed the result of collect()", fixed = TRUE)
  expect_null(rows)
})
