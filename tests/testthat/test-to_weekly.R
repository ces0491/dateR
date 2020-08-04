testthat::test_that("to_weekly works as expected", {

  date_vector <- seq(as.Date("2020-07-01"), as.Date("2020-08-30"), by = "day")

  test_date <- to_weekly(date_vector)
  expected_date <- as.Date(seq(as.Date("2020-07-07"), as.Date("2020-08-25"), by = "week"))

  data_df <- data.frame(date = date_vector, value = c(1:61))
  test_df <- to_weekly(data_df)
  expected_df <- data.frame(date = seq(as.Date("2020-07-07"), as.Date("2020-08-25"), by = "week"),
                            value = c(7, 14, 21, 28, 35, 42, 49, 56))

  testthat::expect_equal(test_date, expected_date)
  testthat::expect_equal(test_df, expected_df)

})
