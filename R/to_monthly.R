#' Convert non-monthly time series data to monthly
#'
#' @param dt_data date data to aggregate to monthly frequency
#' @param ... arguments for other methods
#'
#' @export
#'
to_monthly <- function(dt_data, ...) UseMethod("to_monthly")


#' Convert object of class \code{Date} to monthly periodicity
#'
#' @param dt_data date data to aggregate to monthly frequency
#' @param ... arguments for other methods
#'
#' @export
#'
to_monthly.Date <- function(dt_data, ...) {

  assertR::assert_true(class(dt_data) == "Date")

  dates_diff <- diff(dt_data)

  if (all(dates_diff < 28)) {
    dt_seq <- dt_data
  } else {
    dt_seq <- seq(min(dt_data), max(dt_data), by = "day")
  }

  eom_dts <- get_eom_dates(dt_seq)
  reqd_dates <- unique(eom_dts)

  reqd_dates
}


#' Convert object of class \code{data.frame} to monthly periodicity
#'
#' @param dt_data data.frame with columns date and value
#' @param ... arguments for other methods
#'
#' @export
#'
to_monthly.data.frame <- function(dt_data, ...){

  dates_df <- get_date_col(dt_data)

  assertR::assert_present(names(dates_df), "date")

  dates_diff <- (dates_df$date - dplyr::lag(dates_df$date))[2]

  if (dates_diff < 28) {

    reqd_dates <- condense_dt(dates_df, to_period = "monthly", ...)

  } else {

    reqd_dates <- expand_dt(dates_df, to_period = "monthly", ...)
  }

  reqd_dates
}
