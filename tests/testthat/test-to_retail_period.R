test_that("correct trading dates are returned", {

  retail_calendar <- build_retail_calendar(start_year = 2017, end_year = 2021)

  test_retail_calendar <- retail_calendar %>%
    dplyr::filter(date <= "2021-12-31") %>%
    dplyr::select(date, trading_year, trading_month, trading_quarter, trading_week)

  test_file <- "test_retail_calendar.rds"
  src_dir <- system.file("testdata", package = "dateR")
  src_file <- paste(src_dir, test_file, sep = "/")

  expected_retail_calendar <- readRDS(src_file)

  testthat::expect_equal(test_retail_calendar, expected_retail_calendar)
})
