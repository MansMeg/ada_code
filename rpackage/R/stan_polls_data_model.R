#' Compute the posterior predictive distribution of polls
#' @param spd a [stan_polls] objects
#' @param psd a [poll_of_polls] object
#' @param y_name names of interested parties
#' @param time_scale one what scale is the data
#' @param known_state states where we know the data exactly
#' @param model_time_range what is the time range where the model is active
#' @param latent_time_ranges not certain
#' @param ... extra arguments
#' @export
stan_polls_data_model <- function(spd,
                                  psd,
                                  y_name,
                                  time_scale,
                                  known_state,
                                  model_time_range,
                                  latent_time_ranges,
                                  ...){
  UseMethod("stan_polls_data_model")

}

#' @rdname stan_polls_data_model
#' @export
stan_polls_data_model.model2 <- function(spd, psd, y_name, time_scale = "week", known_state=  NULL, model_time_range = NULL,...){
  checkmate::assert_class(psd, "polls_data")
  checkmate::assert_choice(y_name, choices = names(y(psd)))
  checkmate::assert_choice(time_scale, supported_time_scales())
  assert_time_range(model_time_range, null.ok = TRUE)

  poll_ids <- tibble::tibble(.poll_id = poll_ids(psd), i = 1:length(poll_ids(psd)))
  tl <- get_time_line(psd, model_time_range, time_scale)
  assert_poll_data_in_time_line(psd, tl)

  tws <- polls_time_weights(psd)
  tws <- summarize_polls_time_weights(ptw = tws, tl)
  tws <- dplyr::left_join(tws, tl$time_line, by = "date")
  tws <- dplyr::left_join(tws, poll_ids, by = ".poll_id")

  yvar <- y(psd)[, y_name, drop = TRUE]
  sigma_y <- sqrt(yvar * (1 - yvar) / n(psd))

  sd <- list(T = get_total_time_points_from_time_line(tl),
             N = length(psd),
             L = nrow(tws),
             y = yvar,
             sigma_y = sigma_y,
             tw = tws$weight,
             tw_t = tws$t,
             tw_i = tws$i)

  spd$stan_data <- sd
  spd$time_line <- tl
  spd$poll_ids <- poll_ids
  spd$stan_data$time_scale_length <- time_scale_length(time_scale)
  return(spd)
}

#' @rdname stan_polls_data_model
#' @export
stan_polls_data_model.model3 <- stan_polls_data_model.model2

#' @rdname stan_polls_data_model
#' @export
stan_polls_data_model.model4 <- stan_polls_data_model.model2

#' @rdname stan_polls_data_model
#' @export
stan_polls_data_model.model5 <- function(spd, psd, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL,...){
  spd <- stan_polls_data_model.model2(spd, psd, y_name, time_scale, model_time_range)

  sdks <- stan_data_known_state(y_name, stan_data_time_line = spd$time_line, known_state)
  spd$stan_data$T_known <- sdks$T_known
  spd$stan_data$x_known <- as.array(sdks$x_known[,1])
  spd$stan_data$x_known_t <- sdks$x_known_t
  spd$stan_data$x_unknown_t <- sdks$x_unknown_t


  spd
}
#' @rdname stan_polls_data_model
#' @export
stan_polls_data_model.model6 <- function(spd, psd, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, ...){
  assert_polls_data(psd)
  assert_y_name(y_name, psd)
  assert_time_scale(time_scale)
  assert_time_range(model_time_range, null.ok = TRUE)

  poll_ids <- tibble::tibble(.poll_id = poll_ids(psd), i = 1:length(poll_ids(psd)))
  tl <- get_time_line(psd, model_time_range, time_scale)
  assert_poll_data_in_time_line(psd, tl)

  tws <- polls_time_weights(psd)
  tws <- summarize_polls_time_weights(ptw = tws, tl)
  tws <- dplyr::left_join(tws, tl$time_line, by = "date")
  tws <- dplyr::left_join(tws, poll_ids, by = ".poll_id")

  ymat <- as.matrix(y(psd)[, y_name, drop = FALSE])
  sigma_y <- ymat * (1 - ymat)
  for(i in 1:nrow(sigma_y)){
    sigma_y[i, ] <- sqrt(sigma_y[i, ] / n(psd)[i])
  }

  tsl <- time_scale_length(time_scale)

  sdks <- stan_data_known_state(y_name, stan_data_time_line = tl, known_state)

  sd <- list(T = get_total_time_points_from_time_line(tl),
             N = length(psd),
             L = nrow(tws),
             P = ncol(ymat),
             y = ymat,
             sigma_y = sigma_y,
             tw = tws$weight,
             tw_t = tws$t,
             tw_i = tws$i,
             time_scale_length = tsl,
             T_known = sdks$T_known,
             x_known = sdks$x_known,
             x_known_t = sdks$x_known_t,
             x_unknown_t = sdks$x_unknown_t)
  spd$stan_data <- sd
  spd$time_line <- tl
  spd$poll_ids <- poll_ids
  return(spd)
}

#' Adds missing data through NA to stan data
#' @param stan_data contatning $y which can include missing in the form of na
stan_data_add_missing <- function(stan_data){

  stan_data$y_missing <- 1 * is.na(stan_data$y)
  stan_data$y[stan_data$y_missing == 1] <- 0
  stan_data$sigma_y[stan_data$y_missing == 1] <- 0
  return(stan_data)
}

#' @rdname stan_polls_data_model
#' @export
stan_polls_data_model.model6b <- function(spd, psd, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, ...){

  if(is.null(model_time_range)) model_time_range <- time_range(psd)
  if(is.null(latent_time_ranges)) latent_time_ranges <- setup_latent_time_ranges(x = latent_time_ranges, y = y_name, model_time_range)

  spd <- stan_polls_data_model.model6(spd, psd, y_name, time_scale, known_state, model_time_range)
  spd$stan_data <- stan_data_add_missing(spd$stan_data)

  from_dates <- do.call(c, lapply(latent_time_ranges[y_name], function(x) x["from"]))
  to_dates <- do.call(c, lapply(latent_time_ranges[y_name], function(x) x["to"]))

  spd$stan_data$t_start <- as.array(get_time_points_from_time_line(dates = from_dates, spd$time_line) + 1L)
  spd$stan_data$t_end <- as.array(get_time_points_from_time_line(dates = to_dates, spd$time_line))
  return(spd)
}

#' @rdname stan_polls_data_model
#' @export
stan_polls_data_model.model6c <- stan_polls_data_model.model6b

#' @rdname stan_polls_data_model
#' @export
stan_polls_data_model.model7 <- stan_polls_data_model.model6b

#' @rdname stan_polls_data_model
#' @export
stan_polls_data_model.model9 <- function(spd, psd, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, ...){

  d <- dim(psd$y)[2]-1
  Trans <- svd(t(as.matrix(rep(1,d))), nv=d)$v
  Trans <- sign(Trans[1,1])*Trans
  spd <- stan_polls_data_model.model6b(spd,
                                       psd,
                                       y_name,
                                       time_scale,
                                       known_state,
                                       model_time_range,
                                       latent_time_ranges)
  spd$Trans <- Trans
  spd$stan_data$Transformation <- Trans
  class(spd) <- c("model9", "stan_polls_data")
  spd
}



time_scale_length <- function(time_scale){
  if(time_scale == "day") tsl <- 1
  if(time_scale == "week") tsl <- 7
  if(time_scale == "month") tsl <- 30
  return(tsl)
}


assert_stan_data_model <- function(x){
  UseMethod("assert_stan_data_model")
}

assert_stan_data_model.model2 <- function(x){
  checkmate::assert_integerish(x$stan_data$tw_i, lower = 1, upper = length(x$stan_data$y))
}

assert_stan_data_model.model5 <- function(x){
  assert_stan_data_model.model2(x)

  # Test for data on true latent states
  checkmate::assert_integerish(x$stan_data$T_known, lower = 0, upper = x$stan_data$T)
  checkmate::assert_numeric(x$stan_data$x_known, len = x$stan_data$T_known)
  checkmate::assert_integerish(x$stan_data$x_known_t, len = x$stan_data$T_known, upper = x$stan_data$T)
  checkmate::assert_integerish(x$stan_data$x_unknown_t, len = x$stan_data$T - x$stan_data$T_known, upper = x$stan_data$T)
}


assert_stan_data_model.model3 <- assert_stan_data_model.model2
assert_stan_data_model.model4 <- assert_stan_data_model.model2
assert_stan_data_model.model9 <- assert_stan_data_model.model6
#' function for removing known states outside timeline
#' @param psd [polls_data]
#' @param known_state data.frame containg known result needs date (date of the known data)
#' @param model_time_range time range of where model is active
#' @param time_scale what scale is the data at
#' @export
remove_known_states <- function(psd, known_state, model_time_range = NULL, time_scale = "week"){
  tl <- get_time_line(psd, model_time_range, time_scale)
  dates <- known_state[, "date", drop = TRUE]
  index <- dates %in% tl$daily$date
  return(known_state[index,, drop=F])
}

