# A small sales table with counts that are easy to work out by hand.
#
# region == "APAC" keeps rows 1 to 10.
# year >= 2023 then keeps rows 3, 4, 5, 8, 9 and 10.
# The sum of revenue over those rows is 3900.
make_sales <- function() {
  data.frame(
    customer = rep(c("Acme", "Bolt", "Cove", "Dune"), times = 5),
    region = rep(c("APAC", "EMEA"), each = 10),
    year = rep(2021:2025, times = 4),
    revenue = seq(100, 2000, by = 100),
    stringsAsFactors = FALSE
  )
}

local_dplyr <- function() {
  testthat::skip_if_not_installed("dplyr")
  suppressPackageStartupMessages(library(dplyr, warn.conflicts = FALSE))
}
