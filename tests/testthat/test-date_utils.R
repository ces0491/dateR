testthat::test_that("get date column correctly", {

  date_vector <- seq(as.Date("2020-01-31"), as.Date("2020-12-31"), by = "month")

  data_df <- data.frame(a_strangely_named_date_col = date_vector, variable = rep("XYZ", 12), value = c(1:12))
  test_df <- get_date_col(data_df)

  expected_df <- data_df
  colnames(expected_df) <- c("date", "variable", "value")

  testthat::expect_equal(test_df, expected_df)

})

testthat::test_that("dates correctly have their month end values returned", {

  dates <- c("2020-01-01", "2020-02-14", "2020-06-20")

  test_dates <- get_eom_dates(dates)
  expected_dates <- as.Date(c("2020-01-31", "2020-02-29", "2020-06-30"))

  testthat::expect_equal(test_dates, expected_dates)

})

testthat::test_that("weekends are correctly identified", {

  dates <- seq(from = as.Date("2021-03-01"), to = as.Date("2021-03-07"), by ="day")

  test <- is_weekend(dates)
  expected <- c(rep(FALSE, 5), rep(TRUE, 2))

  testthat::expect_equal(test, expected)

})

testthat::test_that("weekends are correctly removed", {

  dates <- seq(from = as.Date("2021-03-01"), to = as.Date("2021-03-07"), by ="day")

  test_dates <- remove_weekends(dates)
  expected_dates <- seq(from = as.Date("2021-03-01"), to = as.Date("2021-03-05"), by ="day")

  testthat::expect_equal(test_dates, expected_dates)

})
