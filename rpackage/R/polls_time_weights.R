#' @title
#' Compute a time period weight tibble
#'
#' @description
#' Constructs a tibble with the weight of each day of a poll.
#' The tibble can then be aggregated to the right level time
#' scale.
#' The first day of each year, month and week is used to set a
#' date for each time point.
#'
#' @param x a [polls_data] object.
#' @param week_start day on which week starts following ISO
#' conventions - 1 means Monday, 7 means Sunday (default).
#' @param value a [polls_time_weights] object.
#'
#' @return a [polls_time_weights] object (tibble)
#' @export
polls_time_weights <- function(x, ...){
  UseMethod("polls_time_weights")
}


#' @rdname polls_time_weights
#' @export
polls_time_weights.polls_data <- function(x, week_start = 1, ...){
  if(!is.null(x$time_weights)){
    return(x$time_weights)
  }
  sd <- start_dates(x)
  ed <- end_dates(x)
  pid <- poll_ids(x)
  date_has_na <- is.na(sd) | is.na(ed) # Handle missing  values
  dats <- list()
  for(i in 1:length(x)){
    if(date_has_na[i]) next
    dates <- seq(from = sd[i], to = ed[i], by = 1)
    weight <- 1/length(dates)
    dat <- tibble::tibble(.poll_id = pid[i],
                         weight = weight,
                         date = dates)
    dats[[i]] <- dat
  }
  if(any(date_has_na)){
    sna <- sum(date_has_na)
    if(sna > 10){
      warning(sna, " polls are missing start or end date.", call. = FALSE)
    } else {
      warning("The following polls miss start or end date:\n", paste0(pid[date_has_na], collapse = ", "), call. = FALSE)
    }
  }
  dats <- dplyr::bind_rows(dats)
  class(dats) <- c("polls_time_weights", class(dats))
  assert_polls_time_weights(x = dats)
  dats
}

#' @rdname polls_time_weights
#' @export
polls_time_weights.stan_polls_data <- function(x, ...){
  ids <- tibble::tibble(i = x$stan_data$tw_i)
  ids <- dplyr::left_join(ids, x$poll_ids, by = "i")
  ptw  <- tibble::tibble(.poll_id = ids$.poll_id,
                        weight = x$stan_data$tw,
                        date = x$time_line$time_line$date[x$stan_data$tw_t])
  class(ptw) <- c("polls_time_weights", class(ptw))
  assert_polls_time_weights(x = ptw)
  ptw
}

#' Summarize a polls weight object by a time line
#'
#' @param ptw a [polls_time_weights] object.
#' @param tl a [time_line] object.
#'
#' @return a [polls_time_weights] object.
#'
#' @importFrom rlang .data
#'
#' @export
summarize_polls_time_weights <- function(ptw, tl){
  checkmate::assert_class(ptw, classes = "polls_time_weights")
  checkmate::assert_class(tl, classes = "time_line")
  tws <- dplyr::left_join(ptw, tl$daily[, c("date", "time_line_date", "time_line_t")], by = "date")
  tws <- dplyr::group_by(tws, .data$.poll_id, .data$time_line_date)
  tws <- dplyr::summarise(tws, weight = sum(.data$weight))
  names(tws)[2] <- "date"
  tws <- tws[order(as.numeric(tws$.poll_id, tws$date)),]
  tws <- tws[, c(".poll_id", "weight", "date")]
  tws <- dplyr::ungroup(tws)
  class(tws) <- c("polls_time_weights", class(tws))
  assert_polls_time_weights(tws)
  tws
}

assert_polls_time_weights <- function(x){
  checkmate::assert_class(x, "polls_time_weights")
  checkmate::assert_data_frame(x, ncols = 3)
  checkmate::assert_names(names(x), identical.to = c(".poll_id", "weight", "date"))
  checkmate::assert_date(x$date, any.missing = FALSE)
  checkmate::assert_numeric(x$weight, lower = 0, upper = 1)
}



#' @rdname polls_time_weights
#' @export
`polls_time_weights<-` <- function(x, value){
  UseMethod("polls_time_weights<-")
}

#' @rdname polls_time_weights
#' @export
`polls_time_weights<-.polls_data` <- function(x, value){
  if(!is.null(value)){
    checkmate::assert_class(value, "polls_time_weights")
    assert_polls_time_weights(value)
    assert_polls_time_weights_match_polls_data(value, x)
  }
  x$time_weights <- value
  x
}

assert_polls_time_weights_match_polls_data <-
  function(ptw, pd){
    checkmate::assert_class(ptw, "polls_time_weights")
    checkmate::assert_class(pd, "polls_data")
    df <- pd$poll_info[, c(".poll_id", ".start_date", ".end_date")]
    ptw <- dplyr::left_join(ptw, df, by = ".poll_id")
    checkmate::assert_true(all(ptw$date <= ptw$.end_date))
    checkmate::assert_true(all(ptw$date >= ptw$.start_date))

    # Check that all dates are unique and fill out the period
    # Check weights sum to 1 per poll_id
    pids <- unique(poll_ids(pd))
    df$period_length <- as.integer(df$.end_date - df$.start_date) + 1L
    for(i in seq_along(pids)){
      ptw_i <- ptw[ptw$.poll_id == pids[i],]
      df_i <- df[df$.poll_id == pids[i],]
      checkmate::assert_true(!any(duplicated(ptw_i$date)))
      checkmate::assert_true(df_i$period_length == nrow(ptw_i))
      checkmate::assert_true(sum(ptw_i$weight) < 1.00001 & sum(ptw_i$weight) > 0.99999)
    }
  }


#' Reweights the weights  based on function
#'
#' @details
#' The function [FUN] is used to compute the weights
#' for integer values 0,...,T_i, where T_i is the length
#' of the period for poll i. The values from poll is then
#' normalized to sum to 1.
#'
#' @param x a [polls_time_weights] to reweight.
#' @param poll_ids a character vector with poll_ids to reweight.
#' @param FUN function used to reweight
#' @param ... further arguments to [FUN].
#'
#' @export
polls_time_reweight <- function(x, poll_ids, FUN, ...){
  checkmate::assert_class(x, "polls_time_weights")
  checkmate::assert_subset(poll_ids, unique(x$.poll_id))
  checkmate::assert_function(FUN)
  for(i in seq_along(poll_ids)){
    idx <- x$.poll_id == poll_ids[i]
    dom <- 0L:(sum(idx) - 1)
    unnorm <- FUN(dom, ...)
    new_weight <- unnorm / sum(unnorm)
    x$weight[idx] <- new_weight
  }
  assert_polls_time_weights(x)
  x
}
