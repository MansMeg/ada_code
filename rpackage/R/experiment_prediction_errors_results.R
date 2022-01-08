#' Compute error tables
#'
#' Compute prediction tables for a given set of models and the known
#' true values
#'
#' @param x a list of poll of polls models (x$pops)
#' @param known_states a dataframe with known states for the parties
#' @param type type of errors to compute (rmse, quantile, elpd)
#'
#' @export
table_predict_known_state_error <- function(x, known_states, type){
  checkmate::assert_list(x)
  for(i in seq_along(x)){
    checkmate::assert_class(x[[i]], "poll_of_polls")
    checkmate::assert_subset(x[[i]]$y, x[[1]]$y)
  }
  assert_known_state(known_states)
  checkmate::assert_names(names(known_states), must.include = x$y)

  mods <- model_dates_data_frame(x)

  mods$predict_known_state_date <- as.Date(NA)
  for(j in 1:nrow(mods)){
    if(is.na(mods$known_state_end_date[j])) {
      ksed <- mods$model_start_date[j]
    } else {
      ksed <- mods$known_state_end_date[j]
    }
    idx <- which(ksed < known_states$date)
    if(length(idx) == 0) next
    if(length(idx) > 1) idx <- idx[which.min(known_states$date[idx])]
    mods$predict_known_state_date[j] <- known_states[["date"]][idx]
  }

  mods[, x[[1]]$y] <- as.numeric(NA)
  for(j in 1:nrow(mods)){
    if(is.na(mods$predict_known_state_date[j])) next
    pe <-
      compute_prediction_error(x = x[[j]],
                               predict_state = known_states[known_states$date == mods$predict_known_state_date[j],],
                               type)
    mods[j, names(pe)] <- pe
  }
  mods <- mods[!is.na(mods$predict_known_state_date), ]
  mods
}

#' @rdname table_predict_known_state_error
#' @export
predict_known_state_error_table <- function(x, known_states, type){
  .Deprecated("table_predict_known_state_error")
  table_predict_known_state_error(x, known_states, type)
}

#' Compute latent mean coverage
#'
#' Computes coverage of polls with respect to the latent mean
#'
#' @param x a poll of polls object (x$pops)
#' @param standard_errors the number of standard errors to use.
#'
#' @export
table_latent_mean_coverage <- function(x, standard_errors = 1){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_int(standard_errors)

  house_map <- NULL # Just used to pass CHECK tests on defined variables
  utils::data("house_map", envir=environment(), overwrite = TRUE)
  hns <- dplyr::left_join(tibble::tibble(house_name = houses(x$polls_data)), house_map, by = c("house_name"))
  hs <- unique(hns$house)
  gr <- expand.grid(y = x$y, house = hs[!is.na(hs)])
  gr$in_intervals <- -1.0

  for(i in 1:nrow(gr)){
    y <- as.character(gr$y[i])
    house <- unique(hns$house_name[hns$house == gr$house[i]])
    house <- house[!is.na(house)]

    pd <- subset(x$polls_data, subset = x$polls_data$poll_info$.house %in% house)
    dts <- collection_midpoint_dates(pd)
    dts <- latent_state_mean_dates(latent_state(x)[,,y], dts)

    ypd <- y(pd)
    ypd$date <- dts$date
    ypd$mean <- dts$mean
    ypd$st <- (ypd[[y]] - ypd$mean)
    ypd$se <- standard_error(pd, y)[[y]]
    ypd$st_low <- ypd$st - standard_errors * ypd$se
    ypd$st_high <- ypd$st + standard_errors * ypd$se
    ypd$in_interval <- ypd$st_low < 0 & ypd$st_high > 0
    y(pd) <- ypd

    gr$in_intervals[i] <- mean(y(pd)$in_interval, na.rm = TRUE)
  }

  stats::xtabs(in_intervals ~ house+y, data=gr)
}
