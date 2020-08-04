#' Convert non-quarterly time series data to quarterly
#'
#' @param dt_data date data to aggregate to quarterly frequency
#' @param ... arguments for other methods
#'
#' @export
#'
to_quarterly <- function(dt_data, ...) UseMethod("to_quarterly")

#' Convert object of class \code{Date} to quarterly periodicity
#'
#' @param dt_data date data to aggregate to quarterly frequency
#' @param ... arguments for other methods
#'
#' @export
#'
to_quarterly.Date <- function(dt_data, ...) {

  assertR::assert_true(class(dt_data) == "Date")

  dates_diff <- diff(dt_data)

  if (all(dates_diff < 91)) {
    dt_seq <- dt_data
  } else {
    dt_seq <- seq(min(dt_data), max(dt_data), by = "day")
  }

  eom_dt <- get_eom_dates(dt_seq)

  months <- as.numeric(format(eom_dt, "%m"))
  quarters <- which(months %in% c(3, 6, 9, 12))

  reqd_dates <- unique(eom_dt[quarters])

  reqd_dates
}


#' Convert object of class \code{data.frame} to quarterly periodicity
#'
#' @param dt_data date data to aggregate to quarterly frequency
#' @param ... arguments for other methods
#'
#' @export
#'
to_quarterly.data.frame <- function(dt_data, ...) {

  dates_df <- get_date_col(dt_data)

  assertR::assert_present(names(dates_df), "date")

  dates_diff <- (dates_df$date - dplyr::lag(dates_df$date))[2]

  if (dates_diff < 91) {

    reqd_dates <- condense_dt(dates_df, to_period = "quarterly", ...)

  } else {

    reqd_dates <- expand_dt(dates_df, to_period = "quarterly", ...)
  }

  reqd_dates
}
