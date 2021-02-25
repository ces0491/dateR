testthat::test_that("to_weekly works as expected", {

  date_vector <- seq(as.Date("2020-07-01"), as.Date("2020-08-30"), by = "day")

  test_date <- to_weekly(date_vector)
  expected_date <- as.Date(seq(as.Date("2020-07-07"), as.Date("2020-08-25"), by = "week"))

  data_df <- data.frame(id = c(rep("A", 61), rep("B", 61)), date = rep(date_vector, 2), value = c(1:61, 1:61))

  test_df <- data_df %>%
    dplyr::group_by(id) %>%
    to_weekly(.) %>%
    dplyr::ungroup()

  expected_df <- data.frame(id = c(rep("A", 8), rep("B", 8)),
                            date = rep(seq(as.Date("2020-07-07"), as.Date("2020-08-25"), by = "week"), 2),
                            value = rep(c(7, 14, 21, 28, 35, 42, 49, 56), 2))

  testthat::expect_equal(test_date, expected_date)
  testthat::expect_equal(test_df, expected_df)

})
