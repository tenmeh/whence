make_join_data <- function() {
  list(
    x = data.frame(id = 1:5, amount = 11:15, region = c("A", "A", "B", "B", "B")),
    y = data.frame(id = c(1L, 1L, 2L, 3L, 3L, 4L), name = letters[1:6])
  )
}

test_that("joins separate removed rows from repeated matches", {
  local_dplyr()
  data <- make_join_data()
  x <- data$x
  y <- data$y
  expected <- list(
    inner_join = c(n = 6L, delta = -1L, added = 2L),
    left_join = c(n = 7L, delta = 0L, added = 2L),
    semi_join = c(n = 4L, delta = -1L, added = 0L),
    anti_join = c(n = 1L, delta = -4L, added = 0L)
  )
  for (verb in names(expected)) {
    expr <- call("summarise", call(verb, quote(x), quote(y), by = "id"), n = quote(n()))
    tr <- trace_expr(expr, environment())
    expect_identical(unlist(tr$stages[2, c("n", "delta", "added")]), expected[[verb]])
    expect_identical(tr$stages$added[c(1, 3)], c(NA_integer_, NA_integer_))
    expect_equal(tr$stages$percent[[2]], if (verb == "left_join") NA_real_ else expected[[verb]][[2]] * 20)
    expect_identical(tr$stages$label[[2]], paste0(verb, '(y, by = "id")'))
    kept <- if (verb == "left_join") 1:5 else if (verb == "anti_join") 5L else 1:4
    expect_identical(whence_rows(tr), x[kept, ])
    expect_identical(whence_rows(tr, step = 1), x[setdiff(1:5, kept), ])
  }
})

test_that("inner joins drop mutating options from the extra anti join", {
  local_dplyr()
  data <- make_join_data()
  x <- data$x
  y <- data$y
  tr <- whence_trace(
    x |>
      dplyr::inner_join(
        y, "id", FALSE, c(".x", ".y"),
        keep = TRUE, multiple = "all", unmatched = "drop",
        relationship = "one-to-many", na_matches = "never"
      ) |>
      summarise(n = n())
  )
  expect_identical(tr$stages$delta[[2]], -1L)
  expect_identical(tr$stages$added[[2]], 2L)
  expect_identical(whence_rows(tr), x[1:4, ])
})

test_that("natural joins announce their keys once and replay silently", {
  local_dplyr()
  data <- make_join_data()
  x <- data$x
  y <- data$y
  messages <- character()
  tr <- withCallingHandlers(
    whence_trace(x |> inner_join(y) |> summarise(n = n())),
    message = function(cnd) {
      messages <<- c(messages, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  )
  expect_length(messages, 1)
  expect_match(messages, "Joining with", fixed = TRUE)
  expect_message(rows <- whence_rows(tr), NA)
  expect_identical(rows, x[1:4, ])
})

test_that("many-to-many joins warn once and replay without warnings", {
  local_dplyr()
  x <- data.frame(id = c(1L, 1L))
  y <- data.frame(id = c(1L, 1L))
  warnings <- character()
  tr <- withCallingHandlers(
    whence_trace(x |> inner_join(y, by = "id") |> summarise(n = n())),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1)
  expect_match(warnings, "many-to-many", fixed = TRUE)
  expect_warning(rows <- whence_rows(tr), NA)
  expect_identical(rows, x)
})

test_that("warnings from the internal anti join are muffled", {
  local_dplyr()
  x <- data.frame(id = 1:2)
  y <- data.frame(id = 1L)
  anti_join <- function(...) {
    warning("internal anti join", call. = FALSE)
    dplyr::anti_join(...)
  }
  expect_warning(
    tr <- whence_trace(x |> inner_join(y, by = "id") |> summarise(n = n())),
    NA
  )
  expect_identical(tr$stages$delta[[2]], -1L)
})

test_that("a reserved id in y loses attribution with a named reason", {
  local_dplyr()
  data <- make_join_data()
  x <- data$x
  y <- data$y
  y$.whence_id <- "w1"
  for (verb in join_verbs) {
    expr <- call("summarise", call(verb, quote(x), quote(y), by = "id"), n = quote(n()))
    tr <- trace_expr(expr, environment())
    expect_warning(
      rows <- whence_rows(tr),
      paste0("`y` in ", verb, "() has a column named `.whence_id`"),
      fixed = TRUE
    )
    expect_null(rows)
  }
})

test_that("join payloads and printing show both removals and additions", {
  local_dplyr()
  data <- make_join_data()
  x <- data$x
  y <- data$y
  tr <- whence_trace(x |> inner_join(y, by = "id") |> summarise(n = n()))
  payload <- stage_payload(tr$stages[2, ], removable_steps(tr))
  expect_identical(payload$added, 2L)
  expect_identical(payload$addedText, "2")
  expect_identical(payload$removable, TRUE)
  expect_output(print(tr), "-1 rows (20%)  +2 rows from repeated matches", fixed = TRUE)
  expect_null(stage_payload(tr$stages[1, ])$added)
})

test_that("groups after joins keep row attribution and their own added counts", {
  local_dplyr()
  data <- make_join_data()
  x <- data$x
  y <- data$y
  tab <- whence_table(x |> inner_join(y, by = "id") |> group_by(region) |> summarise(n = n()))
  group <- whence_group(tab, region = "B")
  expect_identical(whence_rows(group), x[3:4, ])
  expect_identical(whence_rows(group, step = 1), x[5, ])
  expect_identical(group$stages$n[1:2], c(3L, 3L))
  expect_identical(group$stages$delta[[2]], -1L)
  expect_identical(group$stages$added[[2]], 1L)
  expect_output(print(group), "+1 row from repeated matches", fixed = TRUE)
  payload <- stage_payload(group$stages[2, ])
  expect_identical(payload$added, 1L)
  expect_identical(payload$addedText, "1")
  expect_identical(names(tab$stages), c(
    "step", "label", "n", "delta", "percent", "kind", "verb", "groups", "shown", "added"
  ))
  expect_identical(names(group$stages), names(tab$stages))
  reordered <- tab
  reordered$stages <- tab$stages[rev(names(tab$stages))]
  expect_identical(format(reordered), format(tab))
  expect_identical(stage_payload(reordered$stages[2, ]), stage_payload(tab$stages[2, ]))
  expect_identical(
    group_stages(reordered, group$replay, group$row)[names(tab$stages)],
    group$stages
  )
})

test_that("unmatched and missing keys follow the join's NA policy", {
  local_dplyr()
  x <- data.frame(id = c(1L, NA_integer_))
  y <- data.frame(id = NA_integer_)
  tr <- whence_trace(x |> inner_join(y, by = "id", na_matches = "never") |> summarise(n = n()))
  expect_identical(tr$stages$delta[[2]], -2L)
  expect_identical(tr$stages$added[[2]], 0L)
  expect_identical(whence_rows(tr), x[integer(), , drop = FALSE])
  expect_identical(whence_rows(tr, step = 1), x)
})
