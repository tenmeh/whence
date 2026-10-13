# --- The UI function ---------------------------------------------------------

test_that("explainableOutput() gives the output element with its dependency", {
  tag <- explainableOutput("total", label = "Total")
  html <- as.character(tag)
  deps <- htmltools::findDependencies(tag)

  expect_match(html, 'id="total"', fixed = TRUE)
  expect_match(html, 'class="whence-output"', fixed = TRUE)
  expect_match(html, 'data-label="Total"', fixed = TRUE)
  expect_identical(deps[[1]]$name, "whence")
})

test_that("explainableOutput() checks its arguments", {
  expect_error(explainableOutput(1), "`outputId` must be")
  expect_error(explainableOutput("x", label = 1), "`label` must be")
  expect_error(explainableOutput("x", format = "%.2f"), "`format` must be")
})

test_that("explainableOutput() records format, and NULL removes it", {
  explainableOutput("registry-test", format = toupper)
  expect_identical(format_registry[["registry-test"]], toupper)

  explainableOutput("registry-test")
  expect_null(format_registry[["registry-test"]])
})

# --- The payload ---------------------------------------------------------------

test_that("the payload holds the shown stages, the code and the rows flag", {
  local_dplyr()
  sales <- make_sales()
  tr <- whence_trace(
    sales |>
      filter(region == "APAC") |>
      group_by(customer) |>
      group_by(customer) |>
      summarise(total = sum(revenue)) |>
      summarise(total = sum(total))
  )

  payload <- explain_payload(tr, function(x) paste0("$", x))

  expect_identical(payload$value, "$5500")
  expect_identical(payload$raw, 5500)
  expect_identical(
    vapply(payload$stages, `[[`, "", "kind"),
    c("source", "filter", "group_by", "summarise", "summarise")
  )
  expect_identical(payload$stages[[2]]$delta, -10L)
  expect_identical(payload$stages[[2]]$deltaText, "10")
  expect_null(payload$stages[[4]]$delta)
  expect_identical(payload$rowsAvailable, TRUE)
  expect_null(payload$rowsReason)
})

test_that("the payload names the step past which rows are unavailable", {
  local_dplyr()
  sales <- make_sales()
  tr <- whence_trace(sales |> distinct(region) |> summarise(n = n()))

  payload <- explain_payload(tr)

  expect_identical(payload$rowsAvailable, FALSE)
  expect_identical(payload$rowsReason, "Row attribution unavailable past distinct().")
})

test_that("the default format puts a comma in each thousand", {
  expect_identical(format_value(4281330), "4,281,330")
  expect_identical(format_value("a"), "a")
  expect_error(format_value(1, function(x) c("a", "b")), "single string")
})

# --- The server function ------------------------------------------------------

test_that("renderExplainable() reads inputs and updates when they change", {
  local_dplyr()
  sales <- make_sales()

  server <- function(input, output, session) {
    output$total <- renderExplainable(
      sales |> filter(region == input$region) |> summarise(total = sum(revenue))
    )
  }

  shiny::testServer(server, {
    session$setInputs(region = "APAC")
    expect_identical(output$total$value, "5,500")
    expect_match(output$total$code, 'filter(region == "APAC")', fixed = TRUE)

    session$setInputs(region = "EMEA")
    expect_identical(output$total$value, "15,500")
  })
})

test_that("renderExplainable() accepts a quoted expression", {
  local_dplyr()
  sales <- make_sales()
  expr <- quote(sales |> filter(region == "APAC") |> summarise(n = n()))

  server <- function(input, output, session) {
    output$n <- renderExplainable(expr, quoted = TRUE)
  }

  shiny::testServer(server, {
    expect_identical(output$n$value, "10")
  })
})

# --- In the browser -------------------------------------------------------------

app_path <- function() test_path("apps", "explain")

js <- function(app, code) app$get_js(code)

press <- function(app, key, code = key, keycode = 0, shift = FALSE) {
  cdp <- app$get_chromote_session()
  for (type in c("keyDown", "keyUp")) {
    # A native button acts on Enter only when the key event carries its
    # character, as a real key press does.
    cdp$Input$dispatchKeyEvent(
      type = type, key = key, code = code,
      windowsVirtualKeyCode = keycode,
      modifiers = if (shift) 8 else 0,
      text = if (type == "keyDown" && key == "Enter") "\r" else NULL
    )
  }
}

panel_open <- "document.querySelector('.whence-panel') !== null"

# The first render can take some seconds while the app process starts.
wait_for_output <- function(app, id) {
  app$wait_for_js(
    sprintf("document.querySelector('#%s .whence-value') !== null", id),
    timeout = 20000
  )
}

test_that("a click opens the panel with no trip to the server", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "click")
  wait_for_output(app, "total")

  inputs_before <- names(app$get_values(input = TRUE)$input)
  app$click(selector = "#total .whence-value")
  app$wait_for_js(panel_open)

  expect_identical(names(app$get_values(input = TRUE)$input), inputs_before)
  expect_identical(js(app, "document.querySelector('#total .whence-value').textContent"), "$3,900")
  expect_identical(
    js(app, "Array.from(document.querySelectorAll('.whence-waterfall dt')).map(n => n.textContent)"),
    list("sales", 'region == "APAC"', "year >= 2023", "aggregated to 1 row")
  )
  expect_identical(
    js(app, "Array.from(document.querySelectorAll('.whence-delta')).map(n => n.textContent)"),
    list("\u221210 rows", "\u22124 rows")
  )
  expect_identical(js(app, "document.querySelector('.whence-panel').getAttribute('role')"), "dialog")
  expect_identical(js(app, "document.activeElement.classList.contains('whence-panel')"), TRUE)
  expect_match(js(app, "document.querySelector('.whence-code').textContent"), "filter(year >= 2023)", fixed = TRUE)
  expect_no_shiny_errors(app)
})

test_that("Escape closes the panel, and the focus goes back to the value", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "escape")
  wait_for_output(app, "total")

  app$run_js("document.querySelector('#total .whence-value').focus()")
  press(app, "Enter", keycode = 13)
  app$wait_for_js(panel_open)
  expect_identical(
    js(app, "document.querySelector('#total .whence-value').getAttribute('aria-expanded')"),
    "true"
  )

  press(app, "Escape", keycode = 27)
  app$wait_for_js("document.querySelector('.whence-panel') === null")

  expect_identical(js(app, "document.activeElement.closest('#total') !== null"), TRUE)
  expect_identical(js(app, "document.activeElement.classList.contains('whence-value')"), TRUE)
  expect_no_shiny_errors(app)
})

test_that("Tab keeps the focus in the panel, and the backdrop closes it", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "trap")
  wait_for_output(app, "total")

  app$click(selector = "#total .whence-value")
  app$wait_for_js(panel_open)
  for (i in 1:6) {
    press(app, "Tab", keycode = 9)
  }
  expect_identical(js(app, "document.querySelector('.whence-panel').contains(document.activeElement)"), TRUE)
  press(app, "Tab", keycode = 9, shift = TRUE)
  expect_identical(js(app, "document.querySelector('.whence-panel').contains(document.activeElement)"), TRUE)

  app$click(selector = ".whence-backdrop")
  app$wait_for_js("document.querySelector('.whence-panel') === null")
  expect_identical(js(app, "document.activeElement.classList.contains('whence-value')"), TRUE)
})

test_that("View the rows fires the input and fills the table", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "rows")
  wait_for_output(app, "total")

  app$click(selector = "#total .whence-value")
  app$wait_for_js(panel_open)
  # A keyboard user: the focus is on the button when it is pressed.
  app$run_js("document.querySelector('.whence-rows-button').focus()")
  press(app, "Enter", keycode = 13)
  app$wait_for_js("document.querySelector('.whence-rows h3') !== null")

  inputs <- names(app$get_values(input = TRUE)$input)
  expect_identical(grep("whence_rows", inputs, value = TRUE), "total_whence_rows")
  # The button is gone, so the focus moves to the heading of the rows.
  expect_identical(js(app, "document.activeElement.textContent"), "6 rows")
  expect_identical(js(app, "document.querySelector('.whence-rows h3').textContent"), "6 rows")
  app$wait_for_js("document.querySelector('.whence-rows-content').textContent.indexOf('Acme') >= 0")
  expect_identical(js(app, "document.querySelector('.whence-rows-content').textContent.indexOf('EMEA')"), -1L)
  # reactable is in Suggests. When it is installed, the widget itself must
  # be drawn, and not only its HTML.
  if (requireNamespace("reactable", quietly = TRUE)) {
    expect_identical(js(app, "document.querySelector('.whence-rows-content .ReactTable') !== null"), TRUE)
  }
  expect_no_shiny_errors(app)
})

test_that("past distinct() the panel says that rows are unavailable", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "distinct")
  wait_for_output(app, "customers")

  app$click(selector = "#customers .whence-value")
  app$wait_for_js(panel_open)

  expect_identical(js(app, "document.querySelector('.whence-rows-button') === null"), TRUE)
  expect_identical(
    js(app, "document.querySelector('.whence-rows-note').textContent"),
    "Row attribution unavailable past distinct()."
  )
  expect_no_shiny_errors(app)
})

test_that("a new input value updates the value and closes an open panel", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "update")
  wait_for_output(app, "total")

  app$click(selector = "#total .whence-value")
  app$wait_for_js(panel_open)
  app$set_inputs(region = "EMEA")
  app$wait_for_js("document.querySelector('#total .whence-value').textContent === '$9,900'")

  expect_identical(js(app, panel_open), FALSE)
  expect_no_shiny_errors(app)
})

test_that("on a narrow screen the panel is a sheet at the bottom", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "narrow", width = 400, height = 800)
  wait_for_output(app, "total")

  app$click(selector = "#total .whence-value")
  app$wait_for_js(panel_open)

  expect_identical(js(app, "getComputedStyle(document.querySelector('.whence-panel')).position"), "fixed")
  expect_identical(js(app, "getComputedStyle(document.querySelector('.whence-panel')).bottom"), "0px")
})
test_that("fallback rows payload reports the rows actually shown", {
  local_dplyr()
  d <- data.frame(x = seq_len(500))
  tr <- whence_trace(d |> summarise(n = n()))
  payload <- NULL
  session <- list(sendCustomMessage = function(type, message) payload <<- message)
  send_rows(session, "n", tr, has_reactable = FALSE)
  expect_identical(rows_cap(TRUE), 10000L)
  expect_identical(rows_cap(FALSE), 200L)
  expect_identical(payload$total, 500L)
  expect_identical(payload$shown, 200L)
  expect_identical(payload$shownText, "200")
  expect_length(gregexpr("<td>", payload$html, fixed = TRUE)[[1]], 200L)
  expect_length(gregexpr("<td>", as.character(plain_table(d)), fixed = TRUE)[[1]], 200L)
})
