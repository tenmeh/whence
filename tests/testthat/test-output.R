# --- The UI function ---------------------------------------------------------

test_that("whenceOutput() gives the output element with its dependency", {
  tag <- whenceOutput("total", label = "Total")
  html <- as.character(tag)
  deps <- htmltools::findDependencies(tag)

  expect_match(html, 'id="total"', fixed = TRUE)
  expect_match(html, 'class="whence-output"', fixed = TRUE)
  expect_match(html, 'data-label="Total"', fixed = TRUE)
  expect_identical(deps[[1]]$name, "whence")
})

test_that("whenceOutput() checks its arguments", {
  expect_error(whenceOutput(1), "`outputId` must be")
  expect_error(whenceOutput("x", label = 1), "`label` must be")
})

test_that("renderWhence() checks its arguments", {
  expect_error(renderWhence(1, format = "%.2f"), "`format` must be a function or `NULL`, not character")
  expect_error(renderWhence(1, env = list()), "`env` must be an environment, not list")
  expect_error(renderWhence(quote(1), quoted = "yes"), "`quoted` must be `TRUE` or `FALSE`")
  expect_error(renderWhence(1, outputArgs = "label"), "`outputArgs` must be a list")
})

test_that("renderWhence() passes outputArgs to the output function", {
  render <- renderWhence(1, outputArgs = list(label = "Total"))

  expect_identical(attr(render, "outputArgs"), list(label = "Total"))
  expect_identical(attr(render, "outputFunc"), whenceOutput)
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

test_that("renderWhence() reads inputs and updates when they change", {
  local_dplyr()
  sales <- make_sales()

  server <- function(input, output, session) {
    output$total <- renderWhence(
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

test_that("format runs on the server, and can read inputs", {
  local_dplyr()
  sales <- make_sales()
  currency <- function(x) paste(input_symbol(), format(x, big.mark = ","))
  input_symbol <- NULL

  server <- function(input, output, session) {
    input_symbol <<- function() input$symbol
    output$total <- renderWhence(
      sales |> summarise(total = sum(revenue)),
      format = currency
    )
  }

  shiny::testServer(server, {
    session$setInputs(symbol = "EUR")
    expect_identical(output$total$value, "EUR 21,000")
    expect_identical(output$total$raw, 21000)
  })
})

test_that("two renders with the same output id keep their own format", {
  local_dplyr()
  sales <- make_sales()
  make_server <- function(fmt) {
    function(input, output, session) {
      output$total <- renderWhence(sales |> summarise(n = n()), format = fmt)
    }
  }

  shiny::testServer(make_server(function(x) paste(x, "rows")), {
    expect_identical(output$total$value, "20 rows")
  })
  shiny::testServer(make_server(NULL), {
    expect_identical(output$total$value, "20")
  })
})

# A session that only records the custom messages that it is sent.
fake_session <- function() {
  sent <- list()
  list(
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1]] <<- list(type = type, message = message)
    },
    sent = function() sent
  )
}

test_that("a gone request drops the trace of that output only", {
  traces <- list2env(list(a = "trace a", b = "trace b"))
  session <- fake_session()

  handle_request(session, traces, list(id = "a", action = "gone"))

  expect_identical(sort(ls(traces)), "b")
  expect_length(session$sent(), 0)
})

test_that("a rows request for a dropped trace gets a clear answer", {
  traces <- new.env()
  session <- fake_session()

  handle_request(session, traces, list(id = "a", action = "rows"))

  sent <- session$sent()
  expect_identical(sent[[1]]$type, "whence:rows")
  expect_identical(sent[[1]]$message$ok, FALSE)
  expect_match(sent[[1]]$message$message, "not available any more", fixed = TRUE)
})

test_that("a request with a bad id is ignored", {
  traces <- list2env(list(a = "trace a"))
  session <- fake_session()

  for (id in list(NULL, 1, c("a", "b"), "")) {
    handle_request(session, traces, list(id = id, action = "gone"))
  }

  expect_identical(ls(traces), "a")
  expect_length(session$sent(), 0)
})

test_that("a failure to build the rows is sent to the panel, not raised", {
  traces <- list2env(list(a = structure(list(), class = "whence_trace")))
  session <- fake_session()
  local_mocked_bindings(send_rows = function(...) stop("boom"))

  expect_no_error(handle_request(session, traces, list(id = "a", action = "rows")))
  expect_identical(session$sent()[[1]]$message$ok, FALSE)
})

test_that("every output of a session shares one table of traces", {
  local_dplyr()
  sales <- make_sales()

  server <- function(input, output, session) {
    output$one <- renderWhence(sales |> summarise(n = n()))
    output$two <- renderWhence(sales |> filter(region == "APAC") |> summarise(n = n()))
  }

  shiny::testServer(server, {
    expect_identical(output$one$value, "20")
    expect_identical(output$two$value, "10")
    expect_identical(sort(ls(session$userData$.whence_traces)), c("one", "two"))

    session$setInputs(.whence_request = list(id = "one", action = "gone"))
    expect_identical(ls(session$userData$.whence_traces), "two")
  })
})

test_that("renderWhence() accepts a quoted expression", {
  local_dplyr()
  sales <- make_sales()
  expr <- quote(sales |> filter(region == "APAC") |> summarise(n = n()))

  server <- function(input, output, session) {
    output$n <- renderWhence(expr, quoted = TRUE)
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
  expect_null(app$get_value(input = ".whence_request"))
  expect_identical(js(app, "document.querySelector('#total .whence-value').textContent"), "$3,900")
  expect_identical(
    js(app, "Array.from(document.querySelectorAll('.whence-waterfall dt')).map(n => n.textContent)"),
    list("sales", 'region == "APAC"', "year >= 2023", "aggregated to 1 row")
  )
  expect_identical(
    js(app, "Array.from(document.querySelectorAll('.whence-delta')).map(n => n.textContent)"),
    list("\u221210 rows (50%)", "\u22124 rows (40%)")
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

  expect_identical(app$get_value(input = ".whence_request")$id, "total")
  expect_identical(app$get_value(input = ".whence_request")$action, "rows")
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

test_that("after a join the panel says where the chain stopped", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "joined")
  wait_for_output(app, "joined")

  app$click(selector = "#joined .whence-value")
  app$wait_for_js(panel_open)

  expect_identical(
    js(app, "document.querySelector('.whence-panel .whence-untraced').textContent"),
    "Steps before inner_join() are not shown, because whence does not know inner_join()."
  )
  expect_no_shiny_errors(app)
})

test_that("a removed output closes its panel, and the server drops its trace", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "removed")
  wait_for_output(app, "joined")
  expect_true("joined" %in% app$get_value(export = "traces"))

  app$click(selector = "#joined .whence-value")
  app$wait_for_js(panel_open)
  app$click("drop")
  app$wait_for_js("document.querySelector('.whence-panel') === null")
  app$wait_for_js("document.querySelector('.whence-backdrop') === null")

  # The server is told after a short delay.
  app$wait_for_js("document.getElementById('joined') === null")
  Sys.sleep(1)
  app$wait_for_idle()
  traces <- app$get_value(export = "traces")
  expect_identical(c("joined" %in% traces, "total" %in% traces), c(FALSE, TRUE))
  expect_identical(app$get_value(input = ".whence_request")$action, "gone")
  expect_no_shiny_errors(app)
})

test_that("an output that renderUI() makes again keeps its trace and its rows", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "rebuilt")
  wait_for_output(app, "dynamic_total")

  app$click("rebuild")
  wait_for_output(app, "dynamic_total")
  Sys.sleep(1)
  app$wait_for_idle()
  expect_true("dynamic_total" %in% app$get_value(export = "traces"))

  app$click(selector = "#dynamic_total .whence-value")
  app$wait_for_js(panel_open)
  app$click(selector = ".whence-rows-button")
  app$wait_for_js("document.querySelector('.whence-rows h3') !== null")
  expect_identical(js(app, "document.querySelector('.whence-rows h3').textContent"), "10 rows")
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

test_that("a row of a table opens its own panel, and a removed count shows its rows", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "table")
  app$wait_for_js("document.querySelector('#by_region .whence-table .whence-value') !== null", timeout = 20000)

  expect_identical(
    js(app, "Array.from(document.querySelectorAll('#by_region tbody tr')).map(r => r.textContent)"),
    list("APAC$3,900", "EMEA$9,900")
  )

  app$run_js("document.querySelector('#by_region tbody .whence-value').focus()")
  press(app, "Enter", keycode = 13)
  app$wait_for_js("document.querySelector('.whence-panel .whence-summary') !== null")
  expect_identical(js(app, "document.querySelector('.whence-panel .whence-summary').textContent"), "region = APAC")
  expect_identical(
    js(app, "Array.from(document.querySelectorAll('.whence-waterfall dd')).map(n => n.textContent)"),
    list("10 rows", "6 rows−4 rows (40%)", "6 rows", "1 row")
  )
  expect_match(js(app, "document.querySelector('.whence-code').textContent"), 'filter(region == "APAC")', fixed = TRUE)

  app$run_js("document.querySelector('.whence-delta-button').focus()")
  press(app, "Enter", keycode = 13)
  app$wait_for_js("document.querySelector('.whence-rows h3') !== null")
  expect_identical(js(app, "document.querySelector('.whence-rows h3').textContent"), "4 rows removed by year >= 2023")
  expect_identical(js(app, "document.activeElement.textContent"), "4 rows removed by year >= 2023")

  press(app, "Escape", keycode = 27)
  app$wait_for_js("document.querySelector('.whence-panel') === null")
  expect_identical(js(app, "document.activeElement.textContent"), "APAC")
  expect_no_shiny_errors(app)
})

test_that("a click on a bar of a plot opens the panel of its row", {
  skip_on_cran()
  local_dplyr()
  skip_if_not_installed("ggplot2")
  app <- local_app_driver(app_path(), name = "plot")
  app$wait_for_js("document.querySelector('#plot_region img') !== null", timeout = 20000)
  app$wait_for_js("document.querySelector('#plot_region-whence-table tbody tr') !== null", timeout = 20000)

  # The bar of APAC is the left one, over the lower part of the plot. A
  # mouse event goes to a point of the window, so the plot must be in view.
  app$run_js("document.querySelector('#plot_region').scrollIntoView({block: 'center'})")
  box <- js(app, "(function(){var r = document.querySelector('#plot_region img').getBoundingClientRect();
    return {x: r.left + r.width * 0.33, y: r.top + r.height * 0.75};})()")
  cdp <- app$get_chromote_session()
  for (type in c("mousePressed", "mouseReleased")) {
    cdp$Input$dispatchMouseEvent(type = type, x = box$x, y = box$y, button = "left", clickCount = 1)
  }
  app$wait_for_js("document.querySelector('.whence-panel .whence-summary') !== null", timeout = 10000)

  expect_identical(js(app, "document.querySelector('.whence-panel .whence-summary').textContent"), "region = APAC")
  expect_identical(
    js(app, "document.querySelector('.whence-panel .whence-title').textContent"),
    "Where this row came from"
  )
  expect_null(app$get_value(input = "plot_region_whence_click"))

  press(app, "Escape", keycode = 27)
  app$wait_for_js("document.querySelector('.whence-panel') === null")
  expect_identical(
    js(app, "document.querySelectorAll('#plot_region-whence-table .whence-table .whence-value').length"),
    2L
  )
  expect_no_shiny_errors(app)
})
