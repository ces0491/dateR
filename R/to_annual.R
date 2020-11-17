#' Convert non-annual time series data to annual
#'
#' @param dt_data date data to aggregate to annual frequency
#' @param ... arguments for other methods
#'
#' @export
#'
to_annual <- function(dt_data, ...) UseMethod("to_annual")


#' Convert object of class \code{Date} to annual periodicity
#'
#' @param dt_data date data to aggregate to annual frequency
#' @param ... arguments for other methods
#'
#' @export
#'
to_annual.Date <- function(dt_data, ...) {

  max_date_in <- max(dt_data, na.rm = TRUE)

  years_all <- format(dt_data, "%Y")
  years_unique <- unique(years_all)

  dt_string <- paste(years_unique, 12, 31, sep = "-")
  reqd_dates <- as.Date(dt_string)

  max_date_out <- max(reqd_dates, na.rm = TRUE)

  if(max_date_in < max_date_out) {

    message("max input date: ", max_date_in, " < max output date: ", max_date_out, " truncating sequence...")
    reqd_dates <- reqd_dates[-length(reqd_dates)]
    assertR::assert_true(length(years_unique) - 1 == length(reqd_dates), "annualised date vector size error")
    message("generated date, ", max_date_out, ", dropped from sequence")

  } else {

    assertR::assert_true(length(years_unique) == length(reqd_dates), "annualised date vector size error")

  }

  reqd_dates
}


#' Convert object of class \code{data.frame} to annual periodicity
#'
#' @param dt_data data.frame with columns date and value
#' @param ... arguments for other methods
#'
#' @export
#'
to_annual.data.frame <- function(dt_data, ...) {

  dates_df <- get_date_col(dt_data)

  assertR::assert_present(names(dates_df), "date")

  reqd_dates <- condense_dt(dates_df, to_period = "annual", ...)

  reqd_dates

}
