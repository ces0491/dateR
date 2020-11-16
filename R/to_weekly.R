#' Convert to weekly observations
#'
#' @param dt_data date data to aggregate to weekly frequency
#' @param ... arguments for other methods
#'
#' @export
#'
to_weekly <- function(dt_data, ...) UseMethod("to_weekly")

#' Convert to weekly data observations
#'
#' @param dt_data date data to aggregate to weekly frequency
#' @param ... arguments for other methods
#'
#' @export
#'
to_weekly.Date <- function(dt_data, ...) {

  max_date_in <- max(dt_data)

  weekly_dt <- data.frame(date = dt_data) %>%
    dplyr::mutate(week = as.numeric(date - min(date)) %/% 7) %>% # create period helper variable
    dplyr::group_by(week) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup()

  reqd_dates <- weekly_dt$date

  if((reqd_dates[max(weekly_dt$week)] - reqd_dates[max(weekly_dt$week) - 1]) < 7) {
    max_date_out <- reqd_dates[max(weekly_dt$week) - 1]
  } else{
    max_date_out <- reqd_dates[max(weekly_dt$week)]
  }

  if(max_date_in > max_date_out) {

    message("max input date: ", max_date_in, " is less than a week from the penultimate date in the sequence: ", max_date_out, " truncating sequence...")
    reqd_dates <- reqd_dates[-length(reqd_dates)]
    message("generated date, ", max_date_in, ", dropped from sequence")

  }

  reqd_dates
}

#' Convert to weekly data observations
#'
#' @param dt_data date data to aggregate to weekly frequency
#' @param ... arguments for other methods
#'
#' @export
#'
to_weekly.data.frame <- function(dt_data, ...) {

  dates_df <- get_date_col(dt_data)

  assertR::assert_present(names(dates_df), "date")

  dates_diff <- (dates_df$date - dplyr::lag(dates_df$date))[2]

  if (dates_diff < 7) {

    reqd_dates <- condense_dt(dates_df, to_period = "weekly", ...)

  } else {

    reqd_dates <- expand_dt(dates_df, to_period = "weekly", ...)
  }

  result <- reqd_dates %>%
    dplyr::mutate(dt_diff = date - dplyr::lag(date)) %>%
    tidyr::fill(dt_diff, .direction = "up") %>%
    dplyr::filter(abs(dt_diff) == 7) %>% # dates could be sorted in descending order so date_diff can be negative
    dplyr::select(-dt_diff)

  result
}
