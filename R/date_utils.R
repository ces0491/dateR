#' Get the end of month date
#'
#' @param dates vector of objects of class \code{date} or \code{character} with format Ymd
#'
#' @return vector of dates of the same size of \code{dates} but at month end
#' @export
#'
#' @examples get_eom_dates("2016-01-10")
#'
get_eom_dates <- function(dates) {

  dt_class <- class(dates)
  if(any(dt_class == "character" | dt_class == "POSIXct")) {
    dates <- as.Date(dates)
  }

  assertR::assert_true(class(dates) == "Date", "date format not recognised and could not be converted to date")

  year <- format(dates, "%Y")
  month <- format(dates, "%m")
  dt_string <- paste(year, month, 1, sep = "-")
  fom <- as.Date(dt_string)

  eom_dt <- lubridate::add_with_rollback(fom, lubridate::period("1 month")) - lubridate::days(1)

  eom_dt
}

#' find and rename the date column
#'
#' @param dt_data data.frame containing a column of class \code{Date}
#'
#' @return data.frame with column of class \code{Date} renamed as "date" or NULL
#'
get_date_col <- function(dt_data) {

  df_class_list <- list()
  for(n in 1:length(names(dt_data))) {
    col_class <- class(dt_data[, n])
    col_name <- names(dt_data)[n]
    df_class_list[col_name] <- col_class
  }

  df_class_chr <- unlist(df_class_list)
  dt_col_no <- which(df_class_chr %in% "Date")

  if(length(dt_col_no) == 0) {

    message("no columns of class Date detected")
    dt_data <- NULL

  } else {

    dt_col_name <- names(df_class_chr[dt_col_no])

    assertR::assert_true(length(dt_col_name) == 1, "you may only have 1 date column. if other date variables are required, convert them to char first.")

    names(dt_data)[dt_col_no] <- "date"
  }

  dt_data
}

#' get a vector of dates from \code{data.frame}, \code{zoo}, \code{xts}, or date named vectors
#'
#' @param dt_data object containing date data
#'
#' @return a vector of class \code{Date}
#'
get_date_vector <- function(dt_data) {

  data_class <- class(dt_data)

  if(data_class == "data.frame") {
    dt_df <- get_date_col(dt_data)
    if(is.null(dt_df)) {
      dts <- rownames(dt_data)
    } else {
      dts <- dt_df$date
    }
  }

  if(data_class == "Date") {
    dts <- dt_data
  }

  if(is.vector(dt_data)) {
    dts <- names(dt_data)
  }

  if(data_class == "zoo" | data_class == "xts") {
    dts <- zoo::index(data)
  }

  dt_vec <- try(as.Date(dts), silent = TRUE)

  if(class(dt_vec) == "try-error") {
    stop("unable to extract date vector from dt_data")
  }

  dt_vec

}


#' Determine if a date is a weekend
#'

#' @param dates vector of class \code{Date}
#' @param weekend_days string indicating the days that are classified as weekend, e.g. "Saturday"
#' @param abbreviate logical indicating whether the character vector of weekend days is abbreviated or not
#'
#' @return vector of class \code{logical} indicating weekend days
#' @export
#'
is_weekend <- function(dates, weekend_days = c("Saturday", "Sunday"), abbreviate = FALSE) {

  all_days <- weekdays(as.Date("2020-05-31") + seq(7), abbreviate) #2020/05/31 was a Sunday

  assertR::assert_present(weekend_days, all_days)

  reqd_dates <- weekdays(dates, abbreviate) %in% weekend_days

  reqd_dates
}

#' Remove weekends from the data
#'
#' @param dt_data object containing date data
#' @param weekend_days string indicating the days that are classified as weekend, e.g. "Saturday"
#' @param abbreviate logical indicating whether the character vector of weekend days is abbreviated or not
#'
#' @return data object with weekend days removed
#' @export
#'
remove_weekends <- function(dt_data, weekend_days = c("Saturday", "Sunday"), abbreviate = FALSE) {

  data_index <- get_date_vector(dt_data)

  weekend_dates <- is_weekend(data_index, weekend_days, abbreviate)

  if (length(dim(data)) < 2) {
    reqd_dates <- data[!weekend_dates]
  } else {
    reqd_dates <- data[!weekend_dates, ]
  }

  reqd_dates
}
