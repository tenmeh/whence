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

  app$set_inputs(region = "EMEA", from = 2021)
  app$wait_for_idle()
  expect_no_shiny_errors(app)
})
