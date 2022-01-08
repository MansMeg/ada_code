#' Time range
#'
#' @details
#' Compute the time range for an object with [time_range] or
#' compute the time range of the polls in the object with
#' [time_range_polls].
#'
#' @param x an object to compute the time range for.
#' @param value a Date vector of length 2 with names [from] and [to].
#' @param ... further arguments supplied to method.
#'
#' @export
time_range <- function(x, ...){
  UseMethod("time_range")
}

#' @rdname time_range
#' @export
`time_range<-` <- function(x, value){
  UseMethod("time_range<-")
}

#' @rdname time_range
#' @export
time_range_polls <- function(x, ...){
  UseMethod("time_range_polls")
}

#' @export
time_range.character <- function(x, ...){
  x <- as.Date(x)
  if(is.null(names(x))) names(x) <- c("from", "to")
  return(time_range(x))
}

#' @export
time_range.Date <- function(x, ...){
  if(is.null(names(x))) names(x) <- c("from", "to")
  class(x) <- c("time_range", class(x))
  assert_time_range(x)
  return(x)
}

#' @export
time_range_polls.polls_data <- function(x, ...){
  dates <- c(x$poll_info$.publish_date, x$poll_info$.end_date, x$poll_info$.start_date)
  tr <- c(from = min(dates, na.rm = TRUE), to = max(dates, na.rm = TRUE))
  return(time_range(tr))
}

#' @export
time_range.polls_data <- function(x, ...){
  if(is.null(x$time_range)){
    return(time_range_polls(x))
  } else {
    assert_time_range(x$time_range)
    return(x$time_range)
  }
}

#' @export
`time_range<-.polls_data` <- function(x, value){
  assert_time_range(value)
  x$time_range <- value
  assert_polls_data(x)
  x
}

#' @export
time_range.time_line <- function(x, ...){
  tr <- c(min(x$daily$date), max(x$daily$date))
  names(tr) <- c("from", "to")
  time_range(tr)
}

assert_time_range <- function(x, null.ok = FALSE){
  checkmate::assert_flag(null.ok)
  if(is.null(x) & null.ok) return(x)
  checkmate::assert_class(x, "time_range")
  checkmate::assert_date(x)
  checkmate::assert_names(names(x), identical.to = c("from", "to"))
  checkmate::assert_true(x["from"] <= x["to"])
}

#' Check if two time ranges are disjoint (not overlapping)
#'
#' @param tr1 first [time_range]
#' @param tr2 second [time_range]
#' @export
time_ranges_are_disjoint <- function(tr1, tr2) {
  assert_time_range(tr1)
  assert_time_range(tr2)
  tr1["from"] > tr2["to"] | tr1["to"] < tr2["from"]
}
