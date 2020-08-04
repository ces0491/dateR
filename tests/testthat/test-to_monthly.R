testthat::test_that("to_monthly date conversion works", {

  # aggregate up to monthly
  date_vector <- seq(as.Date("2020-01-01"), as.Date("2020-07-31"), by = "day")

  test_date <- to_monthly(date_vector)
  expected_date <- as.Date(c("2020-01-31", "2020-02-29", "2020-03-31", "2020-04-30", "2020-05-31", "2020-06-30", "2020-07-31"))

  date_df <- data.frame(date = date_vector, value = c(1:213))
  test_df <- to_monthly(date_df)
  expected_df <- data.frame(date = expected_date,
                            value = c(31, 60, 91, 121, 152, 182, 213))

  # aggregate down to monthly
  date_vector_dwn <- seq(as.Date("2019-06-30"), as.Date("2020-06-30"), by = "year")

  test_date_dwn <- to_monthly(date_vector_dwn)
  expected_date_dwn <- as.Date(c("2019-06-30", "2019-07-31", "2019-08-31", "2019-09-30", "2019-10-31", "2019-11-30", "2019-12-31",
                                 "2020-01-31", "2020-02-29", "2020-03-31", "2020-04-30", "2020-05-31", "2020-06-30"))

  data_df_dwn <- data.frame(date = date_vector_dwn, value = c(1:2))
  test_df_dwn <- to_monthly(data_df_dwn)
  expected_df_dwn <- data.frame(date = expected_date_dwn,
                                 value = c(rep(1, 7), rep(2, 6)))

  testthat::expect_equal(test_date, expected_date)
  testthat::expect_equal(test_df, expected_df)
  testthat::expect_equal(test_date_dwn, expected_date_dwn)
  testthat::expect_equal(test_df_dwn, expected_df_dwn)

})

testthat::test_that("to_monthly value aggregation works", {

  date_vector <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")

  data_df_sum <- data.frame(a_strangely_named_date_col = date_vector, value = c(1:31))

  test_df_sum <- to_monthly(data_df_sum, aggregate_by = "sum")
  expected_df_sum <- data.frame(date = as.Date('2020-01-31'),
                                value = sum(1:31))

  test_df_avg <- to_monthly(data_df_sum, aggregate_by = "avg")
  expected_df_avg <- data.frame(date = as.Date('2020-01-31'),
                                value = mean(1:31))

  test_df_median <- to_monthly(data_df_sum, aggregate_by = "median")
  expected_df_median <- data.frame(date = as.Date('2020-01-31'),
                                   value = median(1:31))

  testthat::expect_equal(test_df_sum, expected_df_sum)
  testthat::expect_equal(test_df_avg, expected_df_avg)
  testthat::expect_equal(test_df_median, expected_df_median)

})
