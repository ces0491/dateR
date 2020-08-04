#' convert time series data in a \code{data.frame} to a different frequency
#'
#' @param dt_data date data to aggregate to monthly frequency
#' @param period string indicating the periodicity to convert data to
#' @param aggregate_by string indicating how to aggregate data
#'
#' @return \code{data.frame} or date vector depending on argument \code{dt_data}
#' @export
#'
to_period <- function(dt_data,
                      period = c("weekly", "monthly", "quarterly", "annual"),
                      aggregate_by = c("last", "sum", "avg", "median")) {

  aggregate_by <- match.arg(aggregate_by)

  if(period == "weekly") reqd_dates <- to_weekly(dt_data, aggregate_by)
  if(period == "monthly") reqd_dates <- to_monthly(dt_data, aggregate_by)
  if(period == "quarterly") reqd_dates <- to_quarterly(dt_data, aggregate_by)
  if(period == "annual") reqd_dates <- to_annual(dt_data, aggregate_by)

  reqd_dates
}
