test_that("expected trading calendar is returned", {

  test_trading_calendar <- build_trading_calendar(start_date = '2012-01-01',
                                                  end_date = '2030-12-31',
                                                  trading_month_1 = "April",
                                                  calendar_type = "4-5-4",
                                                  t0 = '2022-03-28')

  test_file <- "test_trading_calendar.rds"
  src_dir <- system.file("testdata", package = "dateR")
  src_file <- paste(src_dir, test_file, sep = "/")

  expected_trading_calendar <- readRDS(src_file)

  testthat::expect_equal(test_trading_calendar, expected_trading_calendar)
})

test_that("expected trading dates are returned", {

  test_trading_period <- get_trading_periods(calendar_dates = '2022-03-31')

  expected_trading_period <- tibble::tibble(TradingYear = 2023,
                                            TradingQuarter = 1,
                                            TradingMonth = 1,
                                            TradingMonthDescription = "April",
                                            TradingWeek = 1,
                                            TradingStartDate = as.Date('2022-03-31'),
                                            TradingEndDate = as.Date('2022-03-31'),
                                            CalendarDate = as.Date('2022-03-31'))

  testthat::expect_equal(test_trading_period, expected_trading_period)
})
