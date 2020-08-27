testthat::test_that("to_period works as expected", {

  date_vector <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")

  test_day <- to_period(date_vector, "daily", "last")
  test_week <- to_period(date_vector, "weekly", "sum")
  test_month <- to_period(date_vector, "monthly", "last")
  test_quart <- to_period(date_vector, "quarterly", "avg")
  test_ann <- to_period(date_vector, "annual", "median")

  expected_week <- seq(as.Date("2020-01-07"), by = "week", length.out = 52)
  expected_month <- get_eom_dates(seq(as.Date("2020-01-01"), by = "month", length.out = 12))
  expected_quart <- as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31"))
  expected_ann <- as.Date("2020-12-31")

  testthat::expect_equal(test_day, date_vector)
  testthat::expect_equal(test_week, expected_week)
  testthat::expect_equal(test_month, expected_month)
  testthat::expect_equal(test_quart, expected_quart)
  testthat::expect_equal(test_ann, expected_ann)

})
