#' build retail calendar
#'
#' @param start_year integer / character specifying the year to start the calendar.
#' @param end_year integer / character specifying the year to end the calendar. Defaults to current year.
#' @param trading_year_start_day integer / character. Integer values must be from 1 to 7, with 1 = Monday and 7 = Sunday. Specifies the day the trading week starts. defaults to Monday.
#' @param trading_year_start_month integer / character. Integer values must be between 1, 12. Specifies the month to check for trading week 1. defaults to January.
#' @param trading_month_start integer / character. Integer values must be between 1, 12. Specifies the month the trading year starts. defaults to April.
#'
#' @return tbl_df with colunms date, calendar_year, calendar_month, calendar_quarter, calendar_week, trading_year, trading_month, trading_quarter, trading_week
#' @export
#'
build_retail_calendar <- function(start_year,
                                   end_year = NULL,
                                   trading_year_start_day = 1,
                                   trading_year_start_month = 1,
                                   trading_month_start = 4) {

  if(is.null(end_year)) {
    end_year <- format(Sys.Date(), "%Y")
  }

  assertR::assert_true(start_year <= end_year, "start year must be before end year")

  # fix date parameters between calendar year start and end (1 Jan - 31 Dec)
  start_dt <- as.Date(paste(start_year, 01, 01, sep = "/"))
  end_dt <- as.Date(paste(end_year, 12, 31, sep = "/"))

  # trading dates will be truncated so extend the calendar date inputs by 1 year on either side
  start_dt <- lubridate::add_with_rollback(start_dt, lubridate::years(-1))
  end_dt_ext <- lubridate::add_with_rollback(end_dt, lubridate::years(1))

  # create recurrence rules for trading calendar start and end
  trading_year_end_day <- dplyr::case_when(trading_year_start_day == 1 ~ 7,
                                           trading_year_start_day == 7 ~ 1,
                                           trading_year_start_day > 1 & trading_year_start_day < 7 ~ trading_year_start_day - 1)

  trading_year_end_month <- dplyr::case_when(trading_year_start_month == 1 ~ 12,
                                             trading_year_start_month == 12 ~ 1,
                                             trading_year_start_month > 1 & trading_year_start_month < 12 ~ trading_year_start_month - 1)

  rule_first_mon <- almanac::yearly() %>%
    almanac::recur_on_ymonth(trading_year_start_month) %>%
    almanac::recur_on_wday(trading_year_start_day, nth = 1)

  rule_last_sun <- almanac::yearly() %>%
    almanac::recur_on_ymonth(trading_year_end_month) %>%
    almanac::recur_on_wday(trading_year_end_day, nth = 4)

  # find dates corresponding to our rules
  first_mondays <- almanac::alma_search(start_dt, end_dt_ext, rule_first_mon)
  last_sundays <- almanac::alma_search(start_dt, end_dt_ext, rule_last_sun)

  assertR::assert_true(length(first_mondays) == length(last_sundays))

  calendar_df <- data.frame(first_mon = first_mondays, last_sun = last_sundays) %>%
    dplyr::mutate(year_end = dateR::get_eom_dates(last_sun)) %>%
    dplyr::mutate(diff = last_sun - first_mon) %>%
    dplyr::mutate(start_trading_week = dplyr::if_else(diff < 356, first_mon - 7, first_mon)) %>%
    dplyr::mutate(diff2 = start_trading_week - dplyr::lag(start_trading_week)) %>%
    dplyr::mutate(start_trading_week = dplyr::if_else(diff2 > 364, first_mon - 7, start_trading_week)) %>%
    dplyr::mutate(end_trading_week = dplyr::lead(start_trading_week) -1) %>%
    tidyr::drop_na() %>%
    dplyr::select(start_trading_week, end_trading_week)

  dt_list <- list()

  # build calendar between trading year start and end dates
  for (n in 1:nrow(calendar_df)) {
    dt_seq <- seq.Date(calendar_df[n, 1], calendar_df[n, 2], by = 'day')
    dt_list[[n]] <- dt_seq
  }

  # get calendar date numerics
  calendar_tbl <- tibble::enframe(dt_list, name = "trading_year_group", value = "date") %>%
    tidyr::unnest(date) %>%
    dplyr::mutate(calendar_year = as.numeric(format(date, "%Y")),
                  calendar_month = as.numeric(format(date, "%m"))
    ) %>%
    dplyr::group_by(calendar_year) %>%
    dplyr::mutate(calendar_quarter = as.numeric((calendar_month - 1) %/% 3) + 1,
                  calendar_week = (as.numeric(date - min(date)) %/% 7) + 1
    ) %>%
    dplyr::ungroup()

  # get trading date numerics
  retail_tbl <- calendar_tbl %>%
    dplyr::mutate(trading_year = ifelse(calendar_month >= trading_month_start, calendar_year + 1, calendar_year)) %>%
    dplyr::mutate(trading_month = ifelse(calendar_month >= trading_month_start,
                                         calendar_month - (trading_month_start - 1),
                                         calendar_month + (12 - trading_month_start + 1))
    ) %>%
    dplyr::mutate(trading_quarter = calendar_quarter) %>%
    dplyr::group_by(trading_year_group) %>%
    dplyr::mutate(trading_week = (as.numeric(date - min(date)) %/% 7) + 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-trading_year_group)

  retail_tbl
}

#' get retail periods
#'
#' @param calendar_dates vector of dates you wish to return retail periods
#' @param format string indicating the format of dates in calendar_dates
#' @param .keep_calendar_dates boolean indicating whether to keep calendar date numerics
#'
#' @return tbl_df
#' @export
#'
get_retail_periods <- function(calendar_dates, format = "%Y-%m-%d", .keep_calendar_dates = FALSE) {

  if(class(calendar_dates) != "Date") {
    calendar_dates <- as.Date(calendar_dates, format)
  }

  start_year <- format(min(calendar_dates), "%Y")

  retail_calendar <- build_retail_calendar(start_year)

  retail_period <- retail_calendar %>%
    dplyr::filter(date %in% calendar_dates)

  if(.keep_calendar_dates) {
    retail_period <- retail_period %>%
      dplyr::select_all()
  } else {
    retail_period <- retail_period %>%
      dplyr::select(!dplyr::starts_with("calendar"))
  }

  retail_period
}
