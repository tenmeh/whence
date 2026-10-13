test_that("the demo app opens each panel and shows the rows, with no errors", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  app_dir <- system.file("examples", "demo", package = "whence")
  skip_if(app_dir == "", "The demo is not installed.")
  app <- local_app_driver(app_dir, name = "demo")

  for (id in c("revenue", "orders", "top_share", "customers")) {
    app$wait_for_js(
      sprintf("document.querySelector('#%s .whence-value') !== null", id),
      timeout = 20000
    )
    app$click(selector = sprintf("#%s .whence-value", id))
    app$wait_for_js("document.querySelector('.whence-panel') !== null")
    if (id != "customers") {
      app$click(selector = ".whence-rows-button")
      app$wait_for_js("document.querySelector('.whence-rows h3') !== null", timeout = 20000)
    }
    app$click(selector = ".whence-close")
    app$wait_for_js("document.querySelector('.whence-panel') === null")
  }

  # A row of the table, and a removed count in its panel.
  app$wait_for_js("document.querySelector('#by_customer .whence-value') !== null", timeout = 20000)
  app$run_js("document.querySelector('#by_customer .whence-value').click()")
  app$wait_for_js("document.querySelector('.whence-panel .whence-delta-button') !== null", timeout = 20000)
  app$run_js("document.querySelector('.whence-panel .whence-delta-button').click()")
  app$wait_for_js("document.querySelector('.whence-rows h3') !== null", timeout = 20000)
  app$click(selector = ".whence-close")
  app$wait_for_js("document.querySelector('.whence-panel') === null")

  # The table under the plot, for the keyboard.
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    app$wait_for_js("document.querySelector('#by_channel-whence-table .whence-value') !== null", timeout = 20000)
    app$run_js("document.querySelector('#by_channel-whence-table .whence-value').click()")
    app$wait_for_js("document.querySelector('.whence-panel .whence-summary') !== null", timeout = 20000)
    app$click(selector = ".whence-close")
  }

  app$set_inputs(region = "EMEA", from = 2021)
  app$wait_for_idle()
  expect_no_shiny_errors(app)
})
