#' Generate 454 trading calendar
#'
#' @param start_date date to begin calendar in Y-m-d format
#' @param end_date date to end calendar in Y-m-d format. defaults to 5 years post start date
#' @param trading_month_1 string indicating the name of month 1
#' @param calendar_type string specifying the calendar type. week numbers should be separated with hyphens. defaults to 4-5-4
#' @param t0 calendar origination date corresponding to day 1, week 1, month 1. THIS SHOULD BE A 52 WEEK YEAR
#'
#' @return tbl_df with cols TradingYear, TradingQuarter, TradingMonth, TradingMonthDescription, TradingWeek, TradingStartDate, TradingEndDate, CalendarDate, CalendarYear, CalendarQuarter, CalendarMonth, CalendarWeek
#' @export
#'
build_trading_calendar <- function(start_date,
                                   end_date = NULL,
                                   trading_month_1 = "April",
                                   calendar_type = "4-5-4",
                                   t0 = '2022-03-28') {

  # currently only calendar type 4-5-4 supported


  assertR::assert_true(
    !is.na(lubridate::parse_date_time(start_date, "%Y-%m-%d"))
    )
  start_date <- as.Date(start_date)

  assertR::assert_true(
    !is.na(lubridate::parse_date_time(t0, "%Y-%m-%d"))
  )
  t0 <- as.Date(t0)

  if(is.null(end_date)) {
    end_date <- lubridate::add_with_rollback(start_date, lubridate::years(5))
  }

  assertR::assert_true(
    !is.na(lubridate::parse_date_time(end_date, "%Y-%m-%d"))
  )
  end_date <- as.Date(end_date)

  assertR::assert_true(start_date <= end_date, "start date must be before end date")
  assertR::assert_true(t0 <= end_date, "origin must be before end date")

  dates <- seq.Date(from = t0, to = end_date, by = "day")

  if(start_date < t0) {
    neg_origin_dates <- seq.Date(from = start_date, to = t0-1, by = "day")
    dates <- c(neg_origin_dates, dates)
  }

  week_num_vec <- rep(c(1:52), each = 7)
  week_dt_vec <- seq.Date(t0, by = "day", length.out = length(week_num_vec))
  week_tbl <- tibble::tibble(date = week_dt_vec,
                             trading_week = week_num_vec)

  month_num_vec_4 <- rep(c(1,3,4,6,7,9,10,12), each = 4*7)
  month_num_vec_5 <- rep(c(2,5,8,11), each = 5*7)
  month_num_vec <- sort(c(month_num_vec_4, month_num_vec_5))
  month_dt_vec <- seq.Date(t0, by = "day", length.out = length(month_num_vec))
  month_tbl <- tibble::tibble(date = month_dt_vec,
                             trading_month = month_num_vec)

  quarter_len <- 4*7 + 5*7 + 4*7
  quarter_num_vec <- rep(c(1:4), each = quarter_len)
  quarter_dt_vec <- seq.Date(t0, by = "day", length.out = length(quarter_num_vec))
  quarter_tbl <- tibble::tibble(date = quarter_dt_vec,
                             trading_quarter = quarter_num_vec)

  if(as.numeric(format(t0, "%m")) > 1) {
    t0_trading_year <- as.numeric(format(t0, "%Y"))+1
  } else {
    t0_trading_year <- as.numeric(format(t0, "%Y"))
  }

  t0_trading_calendar <- week_tbl %>%
    dplyr::left_join(month_tbl, by = "date") %>%
    dplyr::left_join(quarter_tbl, by = "date") %>%
    dplyr::mutate(trading_year = t0_trading_year)


  start_seq <- t0 - (as.numeric(format(t0, "%Y")) - as.numeric(format(start_date, "%Y"))) * 7*52
  end_seq <- t0 + (as.numeric(format(end_date, "%Y"))+1 - as.numeric(format(t0, "%Y"))) * 7*52 - 1
  dt_seq <- seq.Date(from = start_seq, to = end_seq, by = "day")

  dt_seq_df <- data.frame(date = dt_seq)
  dt_seq_df$year <- rep(1:ceiling(nrow(dt_seq_df)/quarter_len*4), each = quarter_len*4)[1:nrow(dt_seq_df)]

  n_years <- as.numeric(format(end_seq, "%Y")) - as.numeric(format(start_seq, "%Y"))
  assertR::assert_true(n_years == max(dt_seq_df$year))

  dt_tbl <- tibble::as_tibble(dt_seq_df) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(
      trading_week = t0_trading_calendar$trading_week,
      trading_month = t0_trading_calendar$trading_month,
      trading_quarter = t0_trading_calendar$trading_quarter,
      trading_year = max(as.numeric(format(date, "%Y")))
    ) %>%
    dplyr::ungroup()

# need to write rules for handling 53rd week historically and forward
  # first need to solve backwards for years which should have started earlier
  # then solve forwards for 53rd week. if the 12th month of the year has 7 days add 53rd week to that year

  month_desc <- format(seq.Date(as.Date('2000-01-01'), as.Date('2000-12-31'), by = 'month'), "%B") # arbitrary dates chosen to get Jan to Dec
  start_month <- which(month_desc %in% trading_month_1)

  month_order <- seq(1, 12 - start_month+1, by = 1)
  if(length(month_order) < 12) {
    rem_seq <- seq(max(month_order)+1,12, by = 1)
    month_order <- c(rem_seq, month_order)
  }

  names(month_order) <- month_desc

  trading_calendar <- dt_tbl %>%
    dplyr::filter(date %in% dates) %>%
    dplyr::arrange(date) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(trading_month_desc = names(which(month_order == trading_month)))

  # rename variables and add gregorian calendar date numerics
  export_calendar <- trading_calendar %>%
    dplyr::mutate(TradingStartDate = date,
                  TradingEndDate = date) %>%
    dplyr::select(trading_year, trading_quarter, trading_month, trading_month_desc, trading_week, TradingStartDate, TradingEndDate, date) %>%
    dplyr::rename(TradingYear = trading_year,
                  TradingQuarter = trading_quarter,
                  TradingMonth = trading_month,
                  TradingMonthDescription = trading_month_desc,
                  TradingWeek = trading_week,
                  CalendarDate = date) %>%
    dplyr::mutate(CalendarYear = as.numeric(format(CalendarDate, "%Y")),
                  CalendarMonth = as.numeric(format(CalendarDate, "%m"))
    ) %>%
    dplyr::group_by(CalendarYear) %>%
    dplyr::mutate(CalendarQuarter = as.numeric((CalendarMonth - 1) %/% 3) + 1,
                  CalendarWeek = (as.numeric(CalendarDate - min(CalendarDate)) %/% 7) + 1
    ) %>%
    dplyr::ungroup()

  export_calendar

}

#' get trading periods
#'
#' @param calendar_dates vector of dates you wish to return trading periods
#' @param format string indicating the format of dates in calendar_dates
#' @param .keep_calendar_dates boolean indicating whether to keep calendar date numerics
#' @param trading_month_1 string indicating the name of month 1
#' @param calendar_type string specifying the calendar type. week numbers should be separated with hyphens. defaults to 4-5-4
#' @param t0 calendar origination date corresponding to day 1, week 1, month 1. THIS SHOULD BE A 52 WEEK YEAR
#'
#' @return tbl_df
#'
get_trading_periods <- function(calendar_dates,
                                format = "%Y-%m-%d",
                                trading_month_1 = "April",
                                calendar_type = "4-5-4",
                                t0 = '2022-03-28',
                                .keep_calendar_dates = FALSE) {

  if(class(calendar_dates) != "Date") {
    calendar_dates <- as.Date(calendar_dates, format)
  }

  start_date <- min(calendar_dates, na.rm = TRUE)
  end_date <- max(calendar_dates, na.rm = TRUE)

  trading_calendar <- build_trading_calendar(start_date, end_date, trading_month_1, calendar_type, t0)

  assertR::assert_present(names(trading_calendar), "CalendarDate")

  trading_period <- trading_calendar %>%
    dplyr::filter(CalendarDate %in% calendar_dates)

  if(.keep_calendar_dates) {
    trading_period <- trading_period %>%
      dplyr::select_all()
  } else {
    trading_period <- trading_period %>%
      dplyr::select(!dplyr::starts_with("Calendar")) %>%
      dplyr::mutate(CalendarDate = TradingEndDate) # we removed all calendar variables but need to keep calendardates
  }

  trading_period
}
