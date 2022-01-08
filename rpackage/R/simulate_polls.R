#' Simulate Polls
#'
#' @description
#' Simulate [npolls] polls for a given latent series [x],
#' where the time slace is defined by [time_scale] and starting
#' from [start_date].
#' Polling periods are randomly sampled from a a [polls_data] object.
#'
#' @param x a latent series to simulate polls for. Either a numeric vector or a data.frame.
#' @param pd a [polls_data] object to sample poll
#'           periods and sample sizes from.
#' @param npolls The total number of polls to simulate
#' @param time_scale The time_scale used, [day], [week], [month] or [year].
#' @param start_date The starting date for the polls.
#'
#' @importFrom rlang .data
#'
#' @export
simulate_polls <- function(x, pd, npolls, time_scale = "week", start_date = as.Date("2010-01-04")){
  if(is.numeric(x)) x <- data.frame(x = x)
  checkmate::assert_data_frame(x, any.missing = FALSE)
  checkmate::assert_class(pd, "polls_data")
  checkmate::assert_int(npolls, lower = 1)
  checkmate::assert_choice(time_scale, c("day", "week", "month", "year"))
  start_date <- as.Date(start_date)
  checkmate::assert_date(start_date)

  # Setup day time line (dtl) and timeline (tl)
  tl <- time_line(x, time_scale, start_date)

  # Sample polls from pd with replacement
  idx <- sample(1:nrow(pd$poll_info), size = npolls, replace = TRUE)
  spd <- pd[idx, keep_poll_id = FALSE]

  # Put polls randomly on time line
  tdiff <- as.integer(nrow(tl$daily) - (publish_dates(spd) - start_dates(spd)))
  prod_length <- as.integer(publish_dates(spd) - end_dates(spd))
  study_length <- as.integer(end_dates(spd) - start_dates(spd))

  start_pos <- floor(stats::runif(n = npolls, min = 0, max = tdiff))
  psd <- start_date + start_pos
  ped <- psd + study_length
  ppd <- ped + prod_length

  # Generate object
  sim_polls <- polls_data(y = data.frame(y=rep(as.numeric(NA), npolls)),
                          house = factor(rep("house", npolls)),
                          publish_date = ppd,
                          start_date = psd,
                          end_date = ped,
                          n = spd$poll_info$.n)
  sim_polls <- sim_polls[order(publish_date(sim_polls), decreasing = TRUE), keep_poll_id = FALSE]
  time_range(sim_polls) <- time_range(c(from=tl$daily[1, "date", drop = TRUE], to = tl$daily[nrow(tl$daily), "date", drop = TRUE]))

  # Compute time_scale weight
  tws <- sample_polls_y(x = sim_polls, true_ls = x, tl = tl, week_start = 1L)
  y(sim_polls) <- tws
  sim_polls
}


#' Sample a dependent variable based on the polls
#'
#' @details
#' The [y] is sampled  based on the [time_weights] and
#' the [true_ls].
#'
#' @param x a [polls_data] object
#' @param true_ls a numeric vector of the length as the [time_line]
#' @param tl a  [time_line] object.
#' @param week_start the integer day starting the week (1 = Monday, 7 = Sunday).
#'
sample_polls_y <- function(x, true_ls, tl, week_start = 1L){
  checkmate::assert_class(x, "polls_data")
  checkmate::assert_class(tl, "time_line")
  checkmate::assert_data_frame(true_ls, nrows = nrow(tl$time_line), any.missing = FALSE)
  for(j in 1:ncol(true_ls)){
    checkmate::assert_numeric(true_ls[[j]], any.missing = FALSE, lower = 0, upper = 1)
  }
  checkmate::assert_int(week_start, lower = 1, upper = 7)

  true_names <- colnames(true_ls)

  tws <- polls_time_weights(x, week_start = week_start)
  tws <- summarize_polls_time_weights(tws, tl)
  tlx <- tl$time_line;
  tlx <- cbind(tlx, true_ls)
  tws <- dplyr::left_join(tws, tlx, by = "date")
  tws <- dplyr::group_by(tws, .data$.poll_id)
  code <- paste0(c("tws <- dplyr::summarise(tws",
                   paste0("'", true_names, "'  = sum(.data$weight*.data[['", true_names,"']])"), ")"), collapse = ", ")
  eval(parse(text = code))
  tws <- tws[order(as.numeric(tws$.poll_id)),]
  for(j in seq_along(true_names)){
    tws[[true_names[j]]] <- stats::rbinom(n = length(x), size = n(x), prob = tws[[true_names[j]]])/n(x)
  }
  tws
}
