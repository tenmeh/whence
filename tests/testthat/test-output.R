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

test_that("marker and hover default to the options and reach the HTML", {
  withr::local_options(whence.marker = NULL, whence.hover = NULL)
  html <- as.character(whenceOutput("x"))
  expect_match(html, 'data-whence-marker="dotted"', fixed = TRUE)
  expect_match(html, 'data-whence-hover="cut"', fixed = TRUE)

  withr::local_options(whence.marker = "dashed", whence.hover = "waterfall")
  html <- as.character(whenceTableOutput("t"))
  expect_match(html, 'data-whence-marker="dashed"', fixed = TRUE)
  expect_match(html, 'data-whence-hover="waterfall"', fixed = TRUE)
  expect_match(
    as.character(whencePlotOutput("p", hover = "none")),
    'data-whence-hover="none"',
    fixed = TRUE
  )
})

test_that("marker and hover are checked strictly", {
  expect_error(
    whenceOutput("x", hover = "cu"),
    '`hover` must be one of "cut", "line", "waterfall", "panel" or "none", not "cu".',
    fixed = TRUE
  )
  expect_error(whenceOutput("x", marker = "Dotted"), "`marker` must be one of", fixed = TRUE)
  expect_error(whenceTableOutput("t", hover = NA), "`hover` must be one of", fixed = TRUE)
  expect_error(whencePlotOutput("p", marker = c("dotted", "dashed")), "`marker` must be one of", fixed = TRUE)
})

test_that("a plot only has a debounced hover input when previews are enabled", {
  html <- as.character(whencePlotOutput("p", hover = "cut"))
  expect_match(html, 'data-hover-id="p_whence_hover"', fixed = TRUE)
  expect_match(html, 'data-hover-delay="300"', fixed = TRUE)
  expect_match(html, 'data-hover-delay-type="debounce"', fixed = TRUE)
  html <- as.character(whencePlotOutput("p", hover = "none"))
  expect_identical(grepl("data-hover-id", html, fixed = TRUE), FALSE)
  expect_match(html, 'data-click-id="p_whence_click"', fixed = TRUE)
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
  tr <- whence_trace(sales |> distinct(r = tolower(region)) |> summarise(n = n()))

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

test_that("each marker style draws its own cue", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "markers")
  style <- function(id, prop) {
    js(app, sprintf("getComputedStyle(document.querySelector('#%s .whence-value')).%s", id, prop))
  }
  wait_for_output(app, "marker_none")
  expect_match(style("revenue", "textDecorationStyle"), "dotted")
  expect_match(style("marker_dashed", "textDecorationStyle"), "dashed")
  expect_identical(js(app, "document.querySelectorAll('#marker_mark .whence-mark').length"), 1L)
  expect_match(style("marker_none", "textDecorationLine"), "none")
  expect_match(style("marker_highlight", "textDecorationStyle"), "dotted")
  expect_identical(js(app, "document.querySelector('#marker_none .whence-value').tabIndex"), 0L)
  expect_no_shiny_errors(app)
})

preview_open <- "document.querySelector('.whence-preview') !== null"

preview_pointer <- function(app, selector, type = "pointerover", pointer = "mouse") {
  app$run_js(sprintf(
    "document.querySelector('%s').dispatchEvent(new PointerEvent('%s', {bubbles: true, pointerType: '%s'}))",
    selector, type, pointer
  ))
}

preview_text <- function(app) {
  js(app, "document.querySelector('.whence-preview').textContent")
}

test_that("the default hover shows the biggest cut and closes on Escape and leave", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-cut")
  wait_for_output(app, "revenue")
  trigger <- "#revenue .whence-value"
  preview_pointer(app, trigger)
  app$wait_for_js(preview_open, timeout = 3000)
  expect_match(preview_text(app), "6 of 20 rows (30%) make this value.", fixed = TRUE)
  expect_match(preview_text(app), 'Biggest cut: region == "APAC"', fixed = TRUE)
  expect_match(preview_text(app), "\u221210 rows (50%)", fixed = TRUE)
  expect_match(preview_text(app), "Click to see every step$")
  expect_identical(js(app, "document.querySelector('.whence-preview').getAttribute('role')"), "tooltip")
  expect_identical(js(app, paste(
    "document.querySelector('#revenue .whence-value').getAttribute('aria-describedby')",
    "=== document.querySelector('.whence-preview').id"
  )), TRUE)
  app$run_js("document.dispatchEvent(new KeyboardEvent('keydown', {key: 'Escape'}))")
  app$wait_for_js(paste0("!(", preview_open, ")"))
  expect_identical(
    js(app, "document.querySelector('#revenue .whence-value').getAttribute('aria-describedby')"),
    "revenue-whence-hint"
  )

  preview_pointer(app, trigger)
  app$wait_for_js(preview_open)
  preview_pointer(app, trigger, "pointerout")
  app$wait_for_js(paste0("!(", preview_open, ")"))
  app$run_js("document.querySelector('#revenue .whence-value').focus()")
  app$wait_for_js(preview_open, timeout = 1000)
  app$run_js("document.querySelector('#revenue .whence-value').blur()")
  expect_identical(js(app, preview_open), FALSE)
  expect_no_shiny_errors(app)
})

test_that("line hover gives a one-line summary on keyboard focus", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-line")
  wait_for_output(app, "hover_line")
  preview_pointer(app, "#hover_line .whence-value")
  app$wait_for_js(preview_open, timeout = 3000)
  expect_match(preview_text(app), "From 20 rows of sales to 6, in 3 steps.", fixed = TRUE)
  preview_pointer(app, "#hover_line .whence-value", "pointerout")
  app$wait_for_js(paste0("!(", preview_open, ")"))
  app$run_js("document.querySelector('#hover_line .whence-value').focus()")
  app$wait_for_js(preview_open, timeout = 1000)
  expect_match(preview_text(app), "From 20 rows of sales to 6, in 3 steps.", fixed = TRUE)
  expect_no_shiny_errors(app)
})

test_that("waterfall hover draws one scaled bar per shown step", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-waterfall")
  wait_for_output(app, "hover_waterfall")
  preview_pointer(app, "#hover_waterfall .whence-value")
  app$wait_for_js(preview_open, timeout = 3000)
  expect_identical(js(app, "document.querySelectorAll('.whence-preview-bar').length"), 4L)
  expect_identical(js(app, paste(
    "Array.from(document.querySelectorAll('.whence-preview-bar'))",
    ".map(function(bar) { return bar.style.width; })"
  )), list("100%", "50%", "30%", "5%"))
  preview_pointer(app, "#hover_waterfall .whence-value", "pointerout")
  app$wait_for_js(paste0("!(", preview_open, ")"))
  app$run_js("document.querySelector('#hover_waterfall .whence-value').focus()")
  app$wait_for_js(preview_open, timeout = 1000)
  expect_identical(js(app, "document.querySelectorAll('.whence-preview-bar').length"), 4L)
  expect_no_shiny_errors(app)
})

test_that("hover none gives no preview on pointer entry or focus", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-none")
  wait_for_output(app, "hover_none")
  preview_pointer(app, "#hover_none .whence-value")
  Sys.sleep(0.45)
  expect_identical(js(app, preview_open), FALSE)
  app$run_js("document.querySelector('#hover_none .whence-value').focus()")
  expect_identical(js(app, preview_open), FALSE)
  expect_no_shiny_errors(app)
})

test_that("a table row preview uses the counts of its group without a request", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-table")
  wait_for_output(app, "by_region")
  preview_pointer(app, "#by_region tbody tr:first-child .whence-value")
  app$wait_for_js(preview_open, timeout = 3000)
  expect_match(preview_text(app), "6 of 10 rows (60%) make this row.", fixed = TRUE)
  expect_match(preview_text(app), "Biggest cut: year >= 2023", fixed = TRUE)
  expect_match(preview_text(app), "\u22124 rows (40%)", fixed = TRUE)
  expect_null(app$get_value(input = ".whence_request"))
  expect_no_shiny_errors(app)
})

test_that("a table row without a count grid shows the table counts and the reason", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-table-fallback")
  wait_for_output(app, "hover_fallback")
  preview_pointer(app, "#hover_fallback tbody tr:first-child .whence-value")
  app$wait_for_js(preview_open, timeout = 3000)
  expect_match(preview_text(app), "2 of 20 rows (10%) make this table.", fixed = TRUE)
  expect_match(preview_text(app), "Counts for this row are not known: the replay stops at distinct().", fixed = TRUE)
  expect_no_shiny_errors(app)
})

test_that("fast moves along three rows leave just the last row preview", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-fast-rows")
  wait_for_output(app, "hover_rows")
  preview_pointer(app, "#hover_rows tbody tr:nth-child(1) .whence-value")
  app$wait_for_js(preview_open, timeout = 3000)
  expect_match(preview_text(app), "3 of 5 rows (60%)", fixed = TRUE)
  app$run_js("
    Array.from(document.querySelectorAll('#hover_rows tbody .whence-value')).slice(0, 3)
      .forEach(function(trigger, i) {
        var options = {bubbles: true, pointerType: 'mouse'};
        if (i !== 0) trigger.dispatchEvent(new PointerEvent('pointerover', options));
        if (i !== 2) trigger.dispatchEvent(new PointerEvent('pointerout', options));
      });
  ")
  app$wait_for_js("Array.from(document.querySelectorAll('.whence-preview')).some(function(node) {
    return node.textContent.indexOf('4 of 5 rows (80%)') >= 0;
  })")
  expect_identical(js(app, "document.querySelectorAll('.whence-preview').length"), 1L)
  expect_no_shiny_errors(app)
})

test_that("a re-render closes the preview and the next hover shows the new counts", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-rerender")
  wait_for_output(app, "hover_update")
  trigger <- "#hover_update .whence-value"
  preview_pointer(app, trigger)
  app$wait_for_js(preview_open, timeout = 3000)
  expect_match(preview_text(app), "12 of 20 rows (60%)", fixed = TRUE)
  app$set_inputs(plot_from = 2022)
  app$wait_for_js(paste0("!(", preview_open, ")"))
  preview_pointer(app, trigger)
  app$wait_for_js(preview_open, timeout = 3000)
  expect_match(preview_text(app), "16 of 20 rows (80%)", fixed = TRUE)
  expect_no_shiny_errors(app)
})

test_that("removeUI closes the preview with its output", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-remove-ui")
  wait_for_output(app, "joined")
  preview_pointer(app, "#joined .whence-value")
  app$wait_for_js(preview_open, timeout = 3000)
  app$run_js("document.querySelector('#drop').click()")
  app$wait_for_js("document.getElementById('joined') === null")
  expect_identical(js(app, preview_open), FALSE)
  expect_no_shiny_errors(app)
})

test_that("the preview of an untraced value gives the trace reason", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-untraced")
  wait_for_output(app, "hover_untraced")
  preview_pointer(app, "#hover_untraced .whence-value")
  app$wait_for_js(preview_open, timeout = 3000)
  expect_match(preview_text(app), "Not traced: the pipeline has no step that whence knows.", fixed = TRUE)
  expect_no_shiny_errors(app)
})

test_that("the preview of a single aggregation says no step removed rows", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-one-step")
  wait_for_output(app, "hover_one")
  preview_pointer(app, "#hover_one .whence-value")
  app$wait_for_js(preview_open, timeout = 3000)
  expect_match(preview_text(app), "20 of 20 rows (100%) make this value.", fixed = TRUE)
  expect_match(preview_text(app), "No step removed rows.", fixed = TRUE)
  expect_no_shiny_errors(app)
})

test_that("a narrow screen gives no hover or focus preview", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-narrow", width = 375, height = 800)
  wait_for_output(app, "revenue")
  preview_pointer(app, "#revenue .whence-value")
  app$run_js("document.querySelector('#revenue .whence-value').focus()")
  Sys.sleep(0.45)
  expect_identical(js(app, preview_open), FALSE)
  expect_no_shiny_errors(app)
})

test_that("a touch pointer gives no preview", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-touch")
  wait_for_output(app, "revenue")
  preview_pointer(app, "#revenue .whence-value", pointer = "touch")
  preview_pointer(app, "#revenue .whence-value", "pointerdown", pointer = "touch")
  app$run_js("document.querySelector('#revenue .whence-value').focus()")
  Sys.sleep(0.45)
  expect_identical(js(app, preview_open), FALSE)
  expect_no_shiny_errors(app)
})

test_that("the panel hover can be entered, and a click pins it", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-panel")
  wait_for_output(app, "hover_panel")
  trigger <- "#hover_panel .whence-value"
  preview_pointer(app, trigger)
  app$wait_for_js("document.querySelector('.whence-preview .whence-summary') !== null", timeout = 3000)
  expect_identical(js(app, "document.querySelector('.whence-backdrop') === null"), TRUE)
  expect_identical(js(app, "document.querySelector('.whence-preview').getAttribute('role')"), "dialog")
  expect_null(js(app, "document.querySelector('.whence-preview').getAttribute('aria-modal')"))
  preview_pointer(app, ".whence-preview")
  preview_pointer(app, trigger, "pointerout")
  Sys.sleep(0.4)
  expect_identical(js(app, preview_open), TRUE)
  app$run_js("document.querySelector('#hover_panel .whence-value').click()")
  app$wait_for_js(paste(panel_open, "&& !(", preview_open, ")"))
  expect_no_shiny_errors(app)
})

test_that("a panel preview opens on keyboard focus and lets focus leave", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-panel-focus")
  wait_for_output(app, "hover_panel")
  app$run_js("document.querySelector('#hover_panel .whence-value').focus()")
  app$wait_for_js(preview_open, timeout = 1000)
  expect_identical(js(app, "document.activeElement.matches('#hover_panel .whence-value')"), TRUE)
  press(app, "Tab", keycode = 9)
  expect_identical(js(app, "document.activeElement.matches('#hover_untraced .whence-value')"), TRUE)
  expect_identical(js(app, "document.querySelector('.whence-preview[role=dialog]') === null"), TRUE)
  expect_identical(js(app, "document.querySelector('.whence-backdrop') === null"), TRUE)
  expect_no_shiny_errors(app)
})

test_that("a table panel hover shows Loading and uses the cached reply next time", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "hover-panel-table")
  wait_for_output(app, "by_region_panel")
  trigger <- "#by_region_panel tbody tr:first-child .whence-value"
  app$run_js("
    window.hoverRequests = [];
    $(document).on('shiny:inputchanged', function(event) {
      if (event.name === '.whence_request' && event.value.action === 'group') {
        var loading = document.querySelector('.whence-preview .whence-loading');
        window.hoverRequests.push(loading && loading.textContent);
      }
    });
  ")
  first_summary <- "document.querySelector('.whence-preview .whence-summary')"
  for (i in 1:2) {
    preview_pointer(app, trigger)
    app$wait_for_js(paste(first_summary, "!== null"), timeout = 3000)
    expect_identical(js(app, paste0(first_summary, ".textContent")), "region = APAC")
    if (i == 1) {
      preview_pointer(app, trigger, "pointerout")
      app$wait_for_js(paste0("!(", preview_open, ")"))
    }
  }
  expect_identical(js(app, "window.hoverRequests"), list("Loading..."))
  app$run_js("document.querySelector('.whence-preview .whence-summary').click()")
  app$wait_for_js(panel_open)
  expect_identical(js(app, "document.querySelector('.whence-panel .whence-summary').textContent"), "region = APAC")
  expect_identical(js(app, "window.hoverRequests"), list("Loading..."))
  expect_identical(js(app, preview_open), FALSE)
  expect_no_shiny_errors(app)
})

test_that("a click opens the panel with no trip to the server", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "click")
  wait_for_output(app, "total")

  inputs_before <- setdiff(names(app$get_values(input = TRUE)$input), ".whence_opened")
  app$click(selector = "#total .whence-value")
  app$wait_for_js(panel_open)

  # The panel is filled from the render: no request goes to the server. The
  # only new input is the notice for whence_opened(), which the panel does
  # not wait for.
  expect_identical(setdiff(names(app$get_values(input = TRUE)$input), ".whence_opened"), inputs_before)
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

test_that("a supported join is shown in the panel", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "joined")
  wait_for_output(app, "joined")

  app$click(selector = "#joined .whence-value")
  app$wait_for_js(panel_open)

  expect_identical(
    js(app, "document.querySelector('.whence-panel .whence-untraced') === null"),
    TRUE
  )
  added <- "document.querySelector('.whence-panel .whence-delta-added')"
  expect_identical(js(app, paste0(added, ".tagName")), "SPAN")
  expect_identical(js(app, paste0(added, ".textContent")), "+10 rows")
  expect_identical(
    js(app, paste0(added, ".title")),
    "10 rows added because a row matched more than once"
  )
  expect_identical(
    js(app, paste0(added, ".getAttribute('aria-label')")),
    "10 rows added because a row matched more than once"
  )
  expect_no_shiny_errors(app)
})

test_that("one repeated match has singular text and accessible labels", {
  skip_on_cran()
  local_dplyr()
  app <- local_app_driver(app_path(), name = "joined-one")
  wait_for_output(app, "joined_one")
  app$click(selector = "#joined_one .whence-value")
  app$wait_for_js(panel_open)
  added <- "document.querySelector('.whence-panel .whence-delta-added')"
  expect_identical(js(app, paste0(added, ".textContent")), "+1 row")
  expect_identical(js(app, paste0(added, ".title")), "1 row added because a row matched more than once")
  expect_identical(
    js(app, paste0(added, ".getAttribute('aria-label')")),
    "1 row added because a row matched more than once"
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
    list("10 rows", "6 rows\u22124 rows (40%)", "6 rows", "1 row")
  )
  expect_match(js(app, "document.querySelector('.whence-code').textContent"), 'filter(region == "APAC")', fixed = TRUE)

  app$run_js("document.querySelector('.whence-delta-button').focus()")
  press(app, "Enter", keycode = 13)
  app$wait_for_js("document.querySelector('.whence-rows h3') !== null")
  expect_identical(js(app, "document.querySelector('.whence-rows h3').textContent"), "4 rows removed by year >= 2023")
  expect_identical(js(app, "document.activeElement.textContent"), "4 rows removed by year >= 2023")

  expect_identical(app$get_value(export = "opened_row"), 1L)
  expect_identical(app$get_value(export = "opened_region"), "APAC")

  press(app, "Escape", keycode = 27)
  app$wait_for_js("document.querySelector('.whence-panel') === null")
  expect_identical(js(app, "document.activeElement.textContent"), "APAC")
  app$wait_for_idle()
  expect_identical(app$get_value(export = "opened_row"), NA_integer_)
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

test_that("a plot that renders again closes its open panel", {
  skip_on_cran()
  local_dplyr()
  skip_if_not_installed("ggplot2")
  app <- local_app_driver(app_path(), name = "plot-again")
  app$wait_for_js("document.querySelector('#plot_region img') !== null", timeout = 20000)
  app$wait_for_js("document.querySelector('#plot_region-whence-table tbody tr') !== null", timeout = 20000)

  app$run_js("document.querySelector('#plot_region-whence-table .whence-value').click()")
  app$wait_for_js("document.querySelector('.whence-panel .whence-summary') !== null", timeout = 10000)
  app$set_inputs(plot_from = 2022)
  app$wait_for_js("document.querySelector('.whence-panel') === null", timeout = 10000)

  expect_identical(js(app, "document.querySelector('.whence-backdrop') === null"), TRUE)
  expect_no_shiny_errors(app)
})

test_that("a plot hover shows the count grid of its bar and closes on NULL", {
  skip_on_cran()
  local_dplyr()
  skip_if_not_installed("ggplot2")
  app <- local_app_driver(app_path(), name = "plot-hover")
  app$wait_for_js("document.querySelector('#plot_region img') !== null", timeout = 20000)
  app$wait_for_js("document.querySelector('#plot_region-whence-table tbody tr') !== null", timeout = 20000)
  app$run_js("document.querySelector('#plot_region').scrollIntoView({block: 'center'})")

  hover <- list(
    x = 1, y = 1000, coords_css = list(x = 132, y = 225),
    mapping = list(x = "reorder(region, total)", y = "total"),
    domain = list(discrete_limits = list(x = list("APAC", "EMEA")))
  )
  counts <- paste(
    "Array.from(document.querySelectorAll('.whence-preview-step span:last-child'))",
    ".map(function(count) { return count.textContent; })"
  )
  app$set_inputs(plot_region_whence_hover = hover, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  app$wait_for_js(preview_open, timeout = 3000)
  expect_identical(js(app, counts), list("10", "6", "6", "1"))
  expect_identical(app$get_value(input = ".whence_request")$action, "hover")

  hover$x <- 2
  hover$coords_css$x <- 265
  app$set_inputs(plot_region_whence_hover = hover, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  app$wait_for_js(paste0("(", counts, ").join('/') === '10/4/4/1'"), timeout = 3000)
  expect_identical(js(app, counts), list("10", "4", "4", "1"))

  app$set_inputs(plot_region_whence_hover = NULL, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  app$wait_for_js(paste0("!(", preview_open, ")"))
  expect_no_shiny_errors(app)
})

test_that("opening 'Show the data' brings the table into view", {
  skip_on_cran()
  local_dplyr()
  skip_if_not_installed("ggplot2")
  app <- local_app_driver(app_path(), name = "plot-data-scroll")
  app$wait_for_js("document.querySelector('#plot_region-whence-table tbody tr') !== null", timeout = 20000)

  # Put the plot far below the fold, then open its data from the top.
  app$run_js("
    document.querySelector('.whence-plot').style.marginTop = '3000px';
    window.scrollTo(0, 0);
    document.querySelector('.whence-plot-data').open = true;
  ")
  in_view <- "(function () {
    var r = document.querySelector('.whence-plot-data').getBoundingClientRect();
    return r.top >= 0 && r.bottom <= window.innerHeight;
  })()"
  app$wait_for_js(in_view, timeout = 5000)

  expect_identical(js(app, in_view), TRUE)
  expect_no_shiny_errors(app)
})
