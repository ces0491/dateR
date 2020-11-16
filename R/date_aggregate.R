#' Condense time series in a data.frame from high to low frequency
#'
#' @param dates_df \code{data.frame} containing a date column and values
#' @param to_period string indicating the periodicity to convert data to
#' @param aggregate_by string indicating how to aggregate data
#'
#' @return data.frame
#'
condense_dt <- function(dates_df,
                        to_period = c("weekly", "monthly", "quarterly", "annual"),
                        aggregate_by = c("last", "sum", "avg", "median")) {

  assertR::assert_present(colnames(dates_df), "date")

  aggregate_by <- match.arg(aggregate_by) # if aggregate_by is not specified, default to the first option, "last"

  dates_ext <- dplyr::mutate(dates_df,
                             week = as.numeric(date - min(date)) %/% 7,
                             month = as.numeric(format(date, "%m")),
                             quarter = as.numeric((month - 1) %/% 3) + 1,
                             year = as.numeric(format(date, "%Y")))

  pvt_agg <- function(x, type = aggregate_by) {
    switch(type,
           sum = sum(x, na.rm = TRUE),
           avg = mean(x, na.rm = TRUE),
           median = stats::median(x, na.rm = TRUE),
           last = x)
  }

  period <- dplyr::case_when(
    to_period == "weekly" ~ "week",
    to_period == "monthly" ~ "month",
    to_period == "quarterly" ~ "quarter")

  group_vars <- stats::na.omit(c("year", period))
  mutate_vars <- setdiff(names(dates_df), c("date", group_vars))

  sub_dates <- dates_ext %>%
    dplyr::group_by_at(group_vars) %>%
    dplyr::mutate_at(mutate_vars, pvt_agg) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup()

  max_date_in <- max(dates_df$date)
  max_date_out <- max(sub_dates$date)

  if(max_date_in < max_date_out) {

    message("max input date: ", max_date_in, " < max output date: ", max_date_out, " truncating sequence...")

    sub_dates <- sub_dates %>%
      dplyr::filter(date != max_date_in)

    message("generated date, ", max_date_out, ", dropped from sequence")

  }

  reqd_dates <- sub_dates %>%
    dplyr::select(-week, -month, -quarter, -year)

  reqd_dates_df <- data.frame(reqd_dates)

  reqd_dates_df

}

#' Expand time series in a data.frame from low to high frequency
#'
#' @param dates_df \code{data.frame} containing a date column and values
#' @param to_period string indicating the periodicity to convert data to
#'
#' @return data.frame
#'
expand_dt <- function(dates_df,
                      to_period = c("weekly", "monthly", "quarterly", "annual")) {

  assertR::assert_present(names(dates_df), "date")

  dts <- dates_df$date
  date_seq <- seq(min(dts), max(dts), by = "day")

  date_seq_df <- data.frame(date = date_seq)

  dates_ext <- dplyr::mutate(date_seq_df,
                             week = as.numeric(date - min(date)) %/% 7,
                             month = as.numeric(format(date, "%m")),
                             quarter = ((month - 1) %/% 3) + 1,
                             year = as.numeric(format(date, "%Y")))

  source_diff <- unclass(diff(dts))

  from_period <- dplyr::case_when(
    source_diff == 7 ~ "week",
    dplyr::between(source_diff, 28, 31) ~ "month",
    dplyr::between(source_diff, 91, 92) ~ "quarter",
    dplyr::between(source_diff, 365, 366) ~ "year"
  )

  period <- dplyr::case_when(
    to_period == "weekly" ~ "week",
    to_period == "monthly" ~ "month",
    to_period == "quarterly" ~ "quarter")

  group_vars <- stats::na.omit(c("year", period))

  daily_dates <- dates_ext %>%
    dplyr::left_join(dates_df, by = "date") %>%   # join the values to the expanded data.frame
    dplyr::group_by_at(dplyr::all_of(from_period)) %>%
    tidyr::fill(-date, -week, -month, -quarter, -year, .direction = "downup") %>%
    dplyr::ungroup()

  reqd_dates_df <- condense_dt(daily_dates, to_period = to_period, aggregate_by = "last")

  reqd_dates_df

}
