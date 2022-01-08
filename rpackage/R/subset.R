#' Subset a [polls_data] object based on a specific [time_range]
#'
#' @description
#' Subset a [polls_data] object.
#' * [subset_publish_dates()] subset the data with the polls
#'   that has  a  publication date within the interval.
#' * [subset_dates()] subset the data with the polls
#'   that has all non-[NA] dates in the interval.
#'
#' @param x a [polls_data] object
#' @param from a the earliest date to include polls for
#' @param to a the latest date to include polls for
#' @param ... further arguments
#'
#' @export
subset_publish_dates <- function(x, from = NULL, to = NULL, ...){
  checkmate::assert_class(x, "polls_data")
  ft <- parse_from_to(from, to, default = time_range(x))
  ind <- publish_dates(x) >= ft["from"] & publish_dates(x) <= ft["to"]
  x <- subset(x, ind)
  time_range(x) <- ft
  as.polls_data(x)
}

#' @rdname subset_publish_dates
#' @export
subset_dates <- function(x, from = NULL, to = NULL, ...){
  checkmate::assert_class(x, "polls_data")
  ft <- parse_from_to(from, to, default = time_range(x))
  pd <- publish_dates(x); sd <- start_dates(x); ed <- end_dates(x)
  min_dates <- sd
  min_dates[is.na(min_dates)] <- ed[is.na(min_dates)]
  min_dates[is.na(min_dates)] <- pd[is.na(min_dates)]
  max_dates <- pd
  ind <- min_dates >= ft["from"] & max_dates <= ft["to"]
  x <- subset(x, subset = ind)
  time_range(x) <- ft
  as.polls_data(x)
}

parse_from_to <- function(from, to, default = NULL, limits = NULL){

  if(!is.null(default)){
    assert_time_range(default)
    if(is.null(from)) from <- as.Date(unname(default["from"]))
    if(is.null(to)) to <- as.Date(unname(default["to"]))
  }
  from <- as.Date(from)
  to <- as.Date(to)
  checkmate::assert_date(from)
  checkmate::assert_date(to)

  if(!is.null(limits)){
    assert_time_range(limits)
    if(from < limits["from"]){
      warning("'from' (",
              from, ") is not >= ",
              limits["from"], ". ",
              limits["from"],
              " is used instead.", call. = FALSE)
    }
    if(to > limits["to"]){
      warning("'to' (",
              to, ") is not <= ",
              limits["to"], ". ",
              limits["to"], ", is used instead.", call. = FALSE)
    }
  }

  ft <- time_range(c(from = from, to = to))
  ft
}


#' Return indicator on polls without missing values
#'
#' @details
#' [complete_poll_info()] indicate polls with complete poll_info.
#'
#' [complete_poll_data()] and [complete_poll_y()] indicate polls with complete poll data (or y)
#'
#' [complete_polls()] indicate polls with complete poll data and info
#'
#' @param x a [polls_data] object.
#'
#' @return a boolean vector
#'
#' @export
complete_poll_info <- function(x){
  checkmate::assert_class(x, "polls_data")
  !is.na(publish_dates(x)) &
    !is.na(start_dates(x)) &
    !is.na(end_dates(x)) &
    !is.na(n(x))
}

#' @rdname complete_poll_info
#' @export
complete_poll_data <- function(x){
  log_mat <- is.na(y(x))
  !apply(log_mat, 1, any)
}

#' @rdname complete_poll_info
#' @export
complete_poll_y <- complete_poll_data

#' @rdname complete_poll_info
#' @export
complete_polls <- function(x){
  complete_poll_data(x) & complete_poll_info(x)
}

#' Subset a latent state based on a time range
#'
#' @param x a [latent_state] object
#' @param from subset from [date]
#' @param to subset to [date]
#' @param ... Currently not in use.
#'
#' @export
subset_latent_state_dates <- function(x, from = NULL, to = NULL, ...){
  checkmate::assert_class(x, "latent_state")
  tr <- time_range(x$time_line)
  ft <- parse_from_to(from, to, default = tr, limits = tr)

  x$time_line <- subset_time_line_dates(x$time_line, from, to)
  ts <- as.character(x$time_line$time_line$t)
  x$latent_state <- x$latent_state[, ts, , drop = FALSE]
  assert_latent_state(x)
  x
}


#' @rdname subset_latent_state_dates
#' @export
subset_time_line_dates <- function(x, from = NULL, to = NULL, ...){
  checkmate::assert_class(x, "time_line")
  ft <- parse_from_to(from, to, default = time_range(x))
  x$daily <- x$daily[x$daily$date >= ft["from"] & x$daily$date <= ft["to"],]
  mind <- min(x$daily$time_line_date)
  maxd <- max(x$daily$time_line_date)
  x$time_line <- x$time_line[x$time_line$date >= mind & x$time_line$date <= maxd,]
  x
}
