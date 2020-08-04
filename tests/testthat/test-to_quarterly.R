testthat::test_that("to_quarterly works as expected", {

  date_vector <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")

  test_date <- to_quarterly(date_vector)
  expected_date <- as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31"))

  data_df <- data.frame(date = date_vector, value = c(1:366))

  test_df <- to_quarterly(data_df)
  expected_df <- data.frame(date = expected_date,
                            value = c(91, 182, 274, 366))

  testthat::expect_equal(test_date, expected_date)
  testthat::expect_equal(test_df, expected_df)

})
