#' Construct a time_line
#'
#' @description
#' Construct a timeline to use based on
#' the [time_scale] of interest.
#'
#' @details
#' The timeline is created based on full months and week.
#' If a [start_date] is supplied, length(x) no of days/weeks/months
#' units are added, including the day/week/month of [start_date].
#' Hence, the number of total days of the [time_line] will change,
#' based on the weekday of [start_date].
#'
#' If [time_range] is supplied, the [time_line] will include all
#' days/weeks/months that is included, fully or partial, by the
#' [time_range].
#'
#' @param x a vector of the same length as the [time_line],
#' a [polls_data] object, or a [time_range] object.
#' @param time_scale The time_scale, [day], [week] or [month].
#' @param start_date The starting date of the [time_line].
#' @param ... Further arguments to methods.
#' @param week_start What is week starting day (1 = Monday, Default or 7 = Sunday).
#'
#' @return a [time_line] object. A list with two data
#' frames, one for a [daily] time_line and one with the
#' [time_line] for the chosen [time_scale].
#' Both data.frames consist of a date variable and
#' a [t] index column.
#'
#' @export
time_line <- function(x, time_scale, ..., week_start = getOption("lubridate.week.start", 1)){
  UseMethod("time_line")
}

#' @export
time_line.polls_data <- function(x, time_scale, ..., week_start = getOption("lubridate.week.start", 1)){
  checkmate::assert_choice(time_scale, supported_time_scales())
  time_line(x = time_range(x), time_scale, week_start = week_start)
}

#' @export
time_line.time_range <- function(x, time_scale, ..., week_start = getOption("lubridate.week.start", 1)){
  checkmate::assert_choice(time_scale, supported_time_scales())

  if(time_scale == "day"){
    n_time_units <- as.integer(x[2] - x[1]) + 1
  } else if (time_scale == "week") {
    xs <- time_scale_dates(x, time_scale, week_start)
    n_time_units <- (as.integer(xs[2] - xs[1]))/7 + 1
  } else if (time_scale == "month") {
    xs <- time_scale_dates(x, time_scale)
    prd <- lubridate::as.period(lubridate::interval(xs[1], xs[2]))
    n_time_units <- lubridate::year(prd) * 12 + lubridate::month(prd) + 1
  } else {
    stop("Not implemented")
  }
  tl <- time_line(x = 1:n_time_units, time_scale, start_date = x[1], ..., week_start = week_start)
  tl$daily <- tl$daily[tl$daily$date <= x[2],]
  tl$time_line <- tl$time_line[tl$time_line$date <= x[2],]
  tl$week_start <- week_start
  assert_time_line(tl)
  tl
}

#' @rdname time_line
#' @export
time_line.numeric <- function(x, time_scale, start_date, ..., week_start = getOption("lubridate.week.start", 1)){
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_choice(time_scale, supported_time_scales())
  start_date <- as.Date(start_date)
  checkmate::assert_date(start_date)

  if(time_scale == "day"){
    dtl <- tibble::tibble(date = (start_date + 0:(length(x) - 1)))
    dtl$t <- 1:nrow(dtl)
    dtl$time_line_date <- time_scale_dates(dtl$date, time_scale)
  } else if (time_scale == "week") {
    start_date_first <- time_scale_dates(start_date, time_scale, week_start)
    end_date <- start_date_first + length(x) * 7 - 1
    dtl <- tibble::tibble(date = seq(from = start_date, to = end_date, by = 1))
    dtl$t <- 1:nrow(dtl)
    dtl$time_line_date <- time_scale_dates(dtl$date, time_scale)
  } else if (time_scale == "month") {
    start_date_first <- time_scale_dates(start_date, time_scale, week_start)
    y <- length(x) %/% 12
    m <- length(x) %% 12
    end_date <- start_date_first + lubridate::years(y) + months(m) - lubridate::days(1)
    dtl <- tibble::tibble(date = seq(from = start_date, to = end_date, by = 1))
    dtl$t <- 1:nrow(dtl)
    dtl$time_line_date <- time_scale_dates(dtl$date, time_scale)
  } else {
    stop("Not implemented")
  }

  tsd <- unique(dtl$time_line_date)
  tsd <- tsd[order(tsd)]
  tl <- tibble::tibble(date = tsd,
                      t = 1:length(tsd),
                      time_line_t = 1:length(tsd))
  dtl <- dplyr::left_join(dtl, tl[,c("date", "time_line_t")], by = c("time_line_date" = "date"))
  tl <- tl[,c("date", "t")]

  tl <- list(daily = dtl, time_line = tl, time_scale = time_scale, week_start = week_start)
  class(tl) <- "time_line"
  assert_time_line(x = tl)
  tl
}

#' @rdname time_line
#' @export
time_line.data.frame <- function(x, time_scale, start_date, ..., week_start = getOption("lubridate.week.start", 1)){
  time_line(x[[1]], time_scale, start_date, ..., week_start)
}

assert_time_line <- function(x){
  checkmate::assert_class(x, "time_line")
  checkmate::assert_names(names(x), must.include = c("daily", "time_line", "time_scale", "week_start"))
  checkmate::assert_data_frame(x$daily)
  checkmate::assert_true(!any(duplicated(x$daily)))
  checkmate::assert_names(names(x$daily), must.include = c("date", "t", "time_line_date", "time_line_t"))
  checkmate::assert_date(x$daily$date)
  checkmate::assert_date(x$daily$time_line_date)

  checkmate::assert_data_frame(x$time_line)
  checkmate::assert_true(!any(duplicated(x$time_line)))
  checkmate::assert_names(names(x$time_line), must.include = c("date", "t"))
  checkmate::assert_date(x$time_line$date)

  checkmate::assert_choice(x$time_scale, supported_time_scales())
}


#' Convert from days to time scale
#'
#' @param x a vector of dates to convert to a specific
#'          [time_scale]
#' @param time_scale to use.
#' @param week_start Day on which week starts following
#'                   ISO conventions - 1 means Monday (default),
#'                   7 means Sunday. You can set lubridate.week.start
#'                   option to control this parameter globally.
#'
#' @export
time_scale_dates <- function(x, time_scale, week_start = getOption("lubridate.week.start", 1)){
  checkmate::assert_date(x)
  checkmate::assert_choice(time_scale, supported_time_scales())
  if(time_scale == "day"){
    dates <- x
  } else if(time_scale == "year"){
    dates <- as.Date(paste(lubridate::year(x), 1, 1, sep="-"), "%Y-%m-%d")
  } else if(time_scale == "month"){
    dates <- as.Date(paste(lubridate::year(x), lubridate::month(x), 1, sep="-"), "%Y-%m-%d")
  } else if(time_scale == "week"){
    dates <- x - lubridate::wday(x, week_start = week_start) + 1
  } else {
    stop("Not implemented.")
  }
  return(dates)
}

supported_time_scales <- function() c("day", "week", "month", "year")

time_scale_as_days <- function(time_scale){
  ds <- c("day" = 1, "week" = 7, "month" = 30, "year" = 365)
  checkmate::assert_choice(time_scale, choices = supported_time_scales())
  checkmate::assert_choice(time_scale, choices = names(ds))
  ds[time_scale]
}

#' Get time points from a time line object
#'
#' @param dates a vector with dates to convert to time points
#' @param tl a [time_line] object
#' @param return_variable the daily variable (time point type) to return
#'
get_time_points_from_time_line <- function(dates, tl, return_variable = "time_line_t"){
  checkmate::assert_date(dates)
  checkmate::assert_class(tl, "time_line")
  assert_dates_in_time_line(dates, tl)
  checkmate::assert_names(names(tl$daily), must.include = return_variable)

  tp <- integer(length(dates))
  for(i in seq_along(dates)){
    idx <- which(dates[i] == tl$daily$date)
    tp[i] <- tl$daily[idx, return_variable, drop = TRUE]
  }
  tp
}

#' Get the time points for specific dates
#'
#' @param x a [time_line] or [poll_of_polls]
#' @param dates a [date] vector
#'
#' @export
get_time_points <- function(x, dates){
  UseMethod("get_time_points")
}

#' @rdname get_time_points
#' @export
get_time_points.time_line <- function(x, dates){
  get_time_points_from_time_line(dates = dates, tl = x, return_variable = "time_line_t")
}

#' @rdname get_time_points
#' @export
get_time_points.poll_of_polls <- function(x, dates){
  get_time_points.time_line(x$time_line, dates)
}

#' @rdname get_time_points_from_time_line
get_total_time_points_from_time_line <- function(tl){
  checkmate::assert_class(tl, "time_line")
  nrow(tl$time_line)
}

#' Assert that the dates are available within the time_line
#' @param dates Dates to check
#' @param tl A [time_line] object to check against
assert_dates_in_time_line <- function(dates, tl){
  checkmate::assert_date(dates)
  checkmate::assert_class(tl, "time_line")
  dates_not_in_timeline <- !dates_in_time_line(dates, tl)
  if(any(dates_not_in_timeline)){
    tr <- time_range(tl)
    stop("The following date(s) are not part of the time_line (", tr[1], " -- ", tr[2], "):\n", paste(dates[dates_not_in_timeline], collapse =", "), call. = FALSE)
  }
}

#' What dates are available within the time_line
#' @param dates Dates to check
#' @param tl A [time_line] object to check against
dates_in_time_line <- function(dates, tl){
  checkmate::assert_date(dates)
  checkmate::assert_class(tl, "time_line")
  dates %in% tl$daily$date
}

#' Assert that the poll_data object dates are available
#' within the time_line
#' @param pd [polls_data] to check if part of [time_line]
#' @param tl [time_line] to check against
assert_poll_data_in_time_line <- function(pd, tl){
  checkmate::assert_class(pd, "polls_data")
  checkmate::assert_class(tl, "time_line")
  assert_dates_in_time_line(publish_dates(pd), tl)
  assert_dates_in_time_line(end_dates(pd), tl)
  assert_dates_in_time_line(start_dates(pd), tl)
}

#' Assert that the poll_data object dates are available
#' within the time_line using time_scale
#'
#' @description
#' The assertion creates a [time_line] using the [time_scale]
#'
#' @seealso
#' [time_range()]
#'
#' @param pd [polls_data] to check if part of [time_line]
#' @param ts a time_scale to use
#' @param tr a [time_range]
assert_poll_data_in_time_line_using_time_scale <- function(pd, ts){
  checkmate::assert_class(pd, "polls_data")
  checkmate::assert_choice(ts, choices = supported_time_scales())
  tl <- time_line(pd, time_scale = ts)
  assert_poll_data_in_time_line(pd, tl)
}

#' @rdname assert_poll_data_in_time_line_using_time_scale
assert_poll_data_in_time_line_using_time_range <- function(pd, ts, tr){
  checkmate::assert_class(pd, "polls_data")
  checkmate::assert_choice(ts, choices = supported_time_scales())
  assert_time_range(tr)
  tl <- time_line(tr, time_scale = ts)
  assert_poll_data_in_time_line(pd, tl)
}

#' Get time line from polls or time range
#'
#' @param x a polls data object
#' @param time_range a time range object
#' @param time_scale a time_scale
get_time_line <- function(x, time_range, time_scale){
  assert_polls_data(x)
  assert_time_range(time_range, null.ok = TRUE)
  assert_time_scale(time_scale)

  if(is.null(time_range)){
    tl <- time_line(x, time_scale)
  } else {
    tl <- time_line(time_range, time_scale)
  }
  tl
}

assert_time_scale <- function(x){
  checkmate::assert_choice(x, choices = supported_time_scales())
}

#' Expand a time_line object to a new time range
#' but keep the same time point index.
#'
#' @param tl a time_line to expand
#' @param time_range a new time range to expand to
#'
time_line_expand <- function(tl, time_range){
  assert_time_line(tl)
  assert_time_range(time_range)

  if(time_line_max_date(tl) > time_range["to"]) time_range["to"] <- time_line_max_date(tl)
  if(time_line_min_date(tl) < time_range["from"]) time_range["from"] <- time_line_min_date(tl)

  new_tl <- time_line(time_range, time_scale = tl$time_scale, week_start = tl$week_start)

  daily_t_new <- new_tl$daily$t[which(new_tl$daily$date == tl$daily$date[1])[1]]
  daily_t_old <- tl$daily$t[1]

  tl_t_new <- new_tl$time_line$t[which(new_tl$time_line$date == tl$time_line$date[1])[1]]
  tl_t_old <- tl$time_line$t[1]

  new_tl$daily$t <- new_tl$daily$t - daily_t_new + daily_t_old
  new_tl$daily$time_line_t <- new_tl$daily$time_line_t - tl_t_new + tl_t_old
  new_tl$time_line$t <- new_tl$time_line$t - tl_t_new + tl_t_old

  new_tl
}

time_line_max_date <- function(tl){
  assert_time_line(tl)
  max(tl$daily$date)
}

time_line_min_date <- function(tl){
  assert_time_line(tl)
  min(tl$daily$date)
}
