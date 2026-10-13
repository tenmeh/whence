# These tests use no dplyr and no shiny. The decomposition works on
# expressions only.

stage_calls <- function(parts) lapply(parts$stages, `[[`, "call")

test_that("a native pipe gives the source and the stages in order", {
  parts <- decompose_pipeline(quote(x |> filter(a) |> summarise(b)))

  expect_identical(parts$source, quote(x))
  expect_identical(
    stage_calls(parts),
    list(quote(filter(.whence_data, a)), quote(summarise(.whence_data, b)))
  )
  expect_identical(
    vapply(parts$stages, `[[`, "", "verb"),
    c("filter", "summarise")
  )
})

test_that("the magrittr pipe gives the same result as the native pipe", {
  native <- decompose_pipeline(quote(x |> filter(a) |> summarise(b)))
  magrittr <- decompose_pipeline(quote(x %>% filter(a) %>% summarise(b)))

  expect_identical(magrittr$source, native$source)
  expect_identical(stage_calls(magrittr), stage_calls(native))
})

test_that("a namespaced verb is a stage", {
  parts <- decompose_pipeline(quote(dplyr::filter(x, a)))

  expect_identical(parts$source, quote(x))
  expect_length(parts$stages, 1)
  expect_identical(parts$stages[[1]]$verb, "filter")
  expect_identical(parts$stages[[1]]$call, quote(dplyr::filter(.whence_data, a)))
})

test_that("a bare symbol after %>% becomes a call", {
  parts <- decompose_pipeline(quote(x %>% collect))

  expect_identical(parts$source, quote(x))
  expect_identical(stage_calls(parts), list(quote(collect(.whence_data))))
})

test_that("an explicit dot takes the data, and nothing is put first", {
  parts <- decompose_pipeline(quote(x %>% f(y, .)))

  expect_identical(stage_calls(parts), list(quote(f(y, .whence_data))))
  expect_identical(parts$stages[[1]]$dot, TRUE)
})

test_that("a dot inside an argument does not stop the data going first", {
  parts <- decompose_pipeline(quote(x %>% f(g(.))))

  expect_identical(stage_calls(parts), list(quote(f(.whence_data, g(.)))))
})

test_that("the native chain stops at a call that is not a verb", {
  parts <- decompose_pipeline(
    quote(x |> filter(a) |> notAVerb() |> summarise(b))
  )

  expect_identical(parts$source, quote(notAVerb(filter(x, a))))
  expect_identical(stage_calls(parts), list(quote(summarise(.whence_data, b))))
})

test_that("a bare name gives a source and no stages", {
  parts <- decompose_pipeline(quote(mtcars))

  expect_identical(parts$source, quote(mtcars))
  expect_length(parts$stages, 0)
})

test_that("decomposition evaluates nothing", {
  expect_no_error(decompose_pipeline(quote(x |> filter(stop("boom")))))
  expect_no_error(decompose_pipeline(quote(stop("boom") %>% filter(a))))
})

test_that("a native placeholder ends the chain safely", {
  # `_` came in R 4.2. Parsed at run time, so that the file still parses on
  # R 4.1, the oldest R that whence supports.
  skip_if(getRversion() < "4.2.0", "The native placeholder needs R 4.2.")
  parts <- decompose_pipeline(str2lang("x |> notAVerb(y = _)"))

  expect_identical(parts$source, quote(notAVerb(y = x)))
  expect_length(parts$stages, 0)
})

test_that("a verb whose first argument is named for something else is not a stage", {
  parts <- decompose_pipeline(quote(head(n = 3, x)))

  expect_length(parts$stages, 0)
})

test_that("a braced block after %>% is not split", {
  expr <- quote(x %>% filter(a) %>% { # nolint: pipe_continuation_linter.
    nrow(.)
  })
  parts <- decompose_pipeline(expr)

  expect_identical(parts$source, expr)
  expect_length(parts$stages, 0)
})

test_that("the display call drops the placeholder", {
  parts <- decompose_pipeline(quote(x %>% filter(a > 1) %>% f(y, .)))

  expect_identical(stage_display_call(parts$stages[[1]]), quote(filter(a > 1)))
  expect_identical(stage_display_call(parts$stages[[2]]), quote(f(y, .)))
})

test_that("decompose.R refers to neither dplyr nor shiny", {
  path <- test_path("..", "..", "R", "decompose.R")
  skip_if_not(file.exists(path), "The package source is not available.")
  code <- readLines(path)
  code <- code[!grepl("^\\s*#", code)]

  expect_identical(grep("dplyr|shiny", code, value = TRUE), character(0))
})
test_that("symbol replacement preserves empty magrittr arguments", {
  parts <- decompose_pipeline(quote(d %>% f(.[, 1])))
  expect_identical(
    replace_symbol(parts$stages[[1]]$call, placeholder_sym, quote(.)),
    quote(f(., .[, 1]))
  )
})

test_that("single expression braces unwrap without evaluation", {
  touched <- FALSE
  parts <- decompose_pipeline(quote({
    stop(touched <- TRUE) |> summarise(n = n())
  }))
  expect_identical(touched, FALSE)
  expect_identical(parts$source, quote(stop(touched <- TRUE)))
  expect_length(parts$stages, 1L)
  block <- quote({
    touched <- TRUE
    d |> summarise(n = n())
  })
  expect_identical(decompose_pipeline(block)$source, block)
  expect_identical(touched, FALSE)
})
