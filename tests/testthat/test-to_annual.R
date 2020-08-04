testthat::test_that("to_annual dates return correctly", {

  date_vector <- seq(as.Date("2015-01-31"), as.Date("2019-12-31"), by = "month")

  test_date <- to_annual(date_vector)
  expected_date <- seq(as.Date("2015-12-31"), as.Date("2019-12-31"), by = "year")

  data_df_long <- data.frame(a_strangely_named_date_col = date_vector, variable = rep("XYZ", 60), value = c(1:60))
  test_df_long <- to_annual(data_df_long)
  expected_df_long <- data.frame(date = seq(as.Date("2015-12-31"), as.Date("2019-12-31"), by = "year"),
                                 variable = rep("XYZ", 5),
                                 value = c(12, 24, 36, 48, 60))

  data_df_wide <- data.frame(a_strangely_named_date_col = date_vector, X = c(1:60), Y = c(1:60)*2, Z = log(1:60))
  test_df_wide <- to_annual(data_df_wide)
  expected_df_wide <- data.frame(date = seq(as.Date("2015-12-31"), as.Date("2019-12-31"), by = "year"),
                                 X = c(12, 24, 36, 48, 60),
                                 Y = 2 * c(12, 24, 36, 48, 60),
                                 Z = log(c(12, 24, 36, 48, 60)))

  testthat::expect_equal(test_date, expected_date)
  testthat::expect_equal(test_df_long, expected_df_long)
  testthat::expect_equal(test_df_wide, expected_df_wide)

})

testthat::test_that("to_annual aggregators work", {

  # we effectively test aggregate_by = "last" in the tests for correct dates above

  date_vector <- seq(as.Date("2018-01-31"), as.Date("2019-12-31"), by = "month")

  data_df_sum <- data.frame(a_strangely_named_date_col = date_vector, value = c(1:24))

  test_df_sum <- to_annual(data_df_sum, aggregate_by = "sum")
  expected_df_sum <- data.frame(date = seq(as.Date("2018-12-31"), as.Date("2019-12-31"), by = "year"),
                                value = c(sum(1:12), sum(13:24)))

  test_df_avg <- to_annual(data_df_sum, aggregate_by = "avg")
  expected_df_avg <- data.frame(date = seq(as.Date("2018-12-31"), as.Date("2019-12-31"), by = "year"),
                                value = c(mean(1:12), mean(13:24)))

  test_df_median <- to_annual(data_df_sum, aggregate_by = "median")
  expected_df_median <- data.frame(date = seq(as.Date("2018-12-31"), as.Date("2019-12-31"), by = "year"),
                                value = c(median(1:12), median(13:24)))

  testthat::expect_equal(test_df_sum, expected_df_sum)
  testthat::expect_equal(test_df_avg, expected_df_avg)
  testthat::expect_equal(test_df_median, expected_df_median)

})
