#' Plot a [polls_data] object
#'
#' @param x a [polls_data] object
#' @param y a variable name in the [polls_data] object.
#' @param publish_date visualize publication dates.
#' @param collection_period visualize collection period.
#' @param ... further arguments to [geom_collection_period]
#' @importFrom graphics plot
#' @export
plot.polls_data <- function(x, y = NULL, publish_date = TRUE, collection_period = TRUE, ...){
  if(is.null(y)) y <- names(y(x))[2]
  checkmate::assert_subset(y, choices = names(y(x))[-1])
  tr <- time_range(x)
  colnames(y(x)) <- make.names(colnames(y(x)))
  y <- make.names(y)
  plt <-
    ggplot2::ggplot()
  if(publish_date){
    plt <- plt + geom_publish_date(x, y, shape = 4, size = 0.5)
  }
  if(collection_period){
    plt <- plt + geom_collection_period(x, y, ...)
  }
  plt <-
    plt +
    ggplot2::xlim(tr["from"], tr["to"]) +
    ggplot2::xlab("")
  plt
}

#' @rdname plot.polls_data
#' @export
geom_publish_date <- function(x, y, ...){
  ggplot2::geom_point(data = as.data.frame(x),
                      ggplot2::aes_string(x = ".publish_date", y = y), ...)
}

#' @rdname plot.polls_data
#' @export
geom_collection_period <- function(x, y, ...){
  ggplot2::geom_segment(data = as.data.frame(x),
                        ggplot2::aes_string(x = ".start_date", xend = ".end_date", y = y, yend = y))
}

#' Add a [latent_state] geom to a ggplot
#'
#' @param x a [latent_state] object
#' @param median should the median be plotted?
#' @param intervals what intervals should be visualized
#' @param latent_state_colour the color of the latent state
#' @param ... further arguments to [geom_ribbon] and [geom_line].
#'
#' @export
geom_latent_state <- function(x, median = TRUE, intervals = c(0.90, 0.75, 0.5), latent_state_colour = "darkgrey", ...){
  checkmate::assert_class(x, "latent_state")
  checkmate::assert_flag(median)
  checkmate::assert_numeric(intervals, lower = 0, upper = 1)


  intervals <- intervals[order(intervals)]
  qs_low <- (1 - intervals)/2
  qs_high <- (1 - (1 - intervals)/2)
  if(median) qs_high <- c(0.5, qs_high)
  ps <- c(rev(qs_low), qs_high)
  psn <- make.names(ps)
  lsp <- latent_state_percentiles(x, percentiles = ps)

  interval_alpha <- 1/(length(intervals) + 1)

  P <- length(psn)
  geom <- list()
  for (i in seq_along(intervals)){
    geom[[i]] <- ggplot2::geom_ribbon(data = lsp, ggplot2::aes_string(x = "date", ymin = psn[i], ymax = psn[P - i + 1]), alpha = interval_alpha, fill = latent_state_colour, ...)
  }
  if(median){
    geom[[length(geom) + 1]] <- ggplot2::geom_line(data = lsp, ggplot2::aes_string(x = "date", y = "X0.5"), colour = latent_state_colour, ...)
  }
  return(geom)
}

#' Visualize a known state in a poll_of_polls ggplot
#' @param x a known_states data.frame
#' @param y the specific variable to visualize
#' @param ... further arguments to geom_line and geom_point
#' @export
geom_known_state <- function(x, y, ...){
  UseMethod("geom_known_state")
}

#' @export
geom_known_state.data.frame <- function(x, y, ...){
  checkmate::assert_data_frame(x)
  checkmate::assert_names(colnames(x), must.include = c("date", y))
  geom <- list()
  colnames(x) <- make.names(colnames(x))
  geom[[1]] <- ggplot2::geom_vline(xintercept = x$date, lty = "dashed", ...)
  geom[[2]] <- ggplot2::geom_point(data = x, ggplot2::aes_string(x = "date", y = make.names(y)), ...)
  geom
}

#' @export
geom_known_state.poll_of_polls <- function(x, y, ...){
  geom_known_state(x$known_state, y, ...)
}

#' Visualize a stan_data object
#' @seealso stan_polls_data
#' @param x a [stan_polls_data] object
#' @param ... further arguments supplied to [geom_polls_time_weights()].
#' @export
geom_stan_polls_data <- function(x, ...){
  checkmate::assert_class(x, "stan_polls_data")
  tws <- polls_time_weights(x)
  tws <- dplyr::left_join(tws, x$poll_ids, by = ".poll_id")
  y <- x$stan_data$y[tws$i]
  geom_polls_time_weights(tws, y, ...)
}


#' @rdname polls_time_weights
#' @param y a vector to plot for the [polls_time_weights] object.
#' @param ... further arguments to [geom_point]
#' @export
geom_polls_time_weights <- function(x, y, ...){
  checkmate::assert_class(x, "polls_time_weights")
  checkmate::assert_numeric(y, len = nrow(x))
  df <- dplyr::bind_cols(x, tibble::tibble(y = y))
  ggplot2::geom_point(data = df, ggplot2::aes(x=date, y = y), ...)
}


#' Plot a [poll_of_polls] object
#'
#' @param x a [poll_of_polls] object
#' @param y column to visualize.
#' @param from plot from [date]
#' @param to plot to [date]
#' @inheritParams plot.polls_data
#'
#' @param ... further arguments to [geom_latent_state]
#' @param shift_latent_days Shift the latent series right, this number of days.
#' @param house only plot the following polling houses
#' @param include_latent_state include latent state in plot
#'
#' @export
plot_poll_of_polls <- function(x, y = NULL, from = NULL, to = NULL, publish_date = TRUE, collection_period = FALSE, shift_latent_days = 0, house = NULL, include_latent_state = TRUE, ...){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_flag(publish_date)
  checkmate::assert_flag(collection_period)
  checkmate::assert_flag(include_latent_state)

  if(is.null(y)) y <- x$y[1]
  checkmate::assert_choice(y, x$y, null.ok = TRUE)

  ls <- latent_state(x, time_line = x$time_line)
  if(!is.null(y)) ls <- ls[, ,y]
  ls <- subset_latent_state_dates(x = ls, from, to)
  ls$time_line$time_line$date <- ls$time_line$time_line$date + lubridate::days(shift_latent_days)

  pd <- x$polls_data
  if(!is.null(house)){
    checkmate::assert_subset(house, levels(x$polls_data$poll_info$.house))
    pd <- subset(x$polls_data, subset = houses(x$polls_data) %in% house)
  }
  pd <- subset_dates(x = pd, from, to)

  ft <- parse_from_to(from, to,
                      default = time_range(ls$time_line),
                      limits = time_range(ls$time_line))

  x$known_state <- x$known_state[x$known_state$date >= ft["from"] & x$known_state$date <= ft["to"], c("date", y)]

  plt <- plot.polls_data(x = pd, y = y, publish_date = publish_date, collection_period = FALSE)
  if(collection_period & nrow(y(pd)) > 0){
    sd <- stan_polls_data(x = pd,
                          y_name = y,
                          time_scale = x$time_scale,
                          model = x$model,
                          known_state = x$known_state,
                          model_time_range = time_range(x$time_line),
                          latent_time_ranges = x$latent_time_range)
    plt <- plt + geom_stan_polls_data(sd, size = 0.3, alpha = 0.5)
  }
  if(include_latent_state){
    plt <- plt + geom_latent_state(x = ls, ...)
  }
  plt
}

#' @rdname plot_poll_of_polls
#' @export
plot.poll_of_polls <- function(x, ...){
  plot_poll_of_polls(x, ...)
}

#' @rdname plot_poll_of_polls
#' @param ... further arguments to [geom_line]
#' @export
geom_pop_line <- function(x, y, ...){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_numeric(x = y, len = length(x$time_line$time_line$date))
  ggplot2::geom_line(data = data.frame(date = x$time_line$time_line$date,
                                       y = y),
                     ggplot2::aes(x=date, y = y), ...)
}


#' @rdname plot_poll_of_polls
#' @param house the house or houses to normalize and plot.
#' @param standard_errors logical flag indicating if standard error of polls should be visualized
#' @export
plot_house_bias <- function(x, y, house, publish_date = FALSE, standard_errors = TRUE){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_choice(y, choices = x$y)
  checkmate::assert_subset(house, levels(x$polls_data$poll_info$.house))
  checkmate::assert_flag(publish_date)
  checkmate::assert_flag(standard_errors)

  pd <- subset(x$polls_data, subset = x$polls_data$poll_info$.house %in% house)

  dts <- collection_midpoint_dates(pd)
  dts <- latent_state_mean_dates(latent_state(x)[,,y], dts)

  ypd <- y(pd)
  ypd$date <- dts$date
  ypd$mean <- dts$mean
  ypd$st <- (ypd[[y]] - ypd$mean)
  ypd$se <- standard_error(pd, y)[[y]]
  ypd$st_low <- ypd$st - ypd$se
  ypd$st_high <- ypd$st + ypd$se
  ypd$in_interval <- ypd$st_low < 0 & ypd$st_high > 0
  y(pd) <- ypd

  pd <- subset(pd, subset = abs(ypd$st) < Inf)
  df <- as.data.frame(pd)

  ks <- x$known_state
  ks$x <- 0
  l <- max(abs(y(pd)$st))
  if(standard_errors) {
    l  <- max(abs(c(y(pd)$st_high, y(pd)$st_low)))
    label <- paste0("P(0 in interval): ", round(mean(y(pd)$in_interval), 3), "")
  } else {
#    subtitle <- NULL
  }
  ks[[y]] <- 0
  plt <- plot.polls_data(pd, "st", publish_date = publish_date) +
    geom_known_state(ks, y) +
    ggplot2::ylim(c(-l, l)) +
    ggplot2::ylab(y) +
    ggplot2::ggtitle(house[1]) +
    ggplot2::geom_hline(yintercept = 0, lty = "dotted")
  if(standard_errors){
    plt <- plt +
      ggplot2::geom_segment(data = df, ggplot2::aes_string(x = "date", xend = "date", y = "st_low", yend = "st_high")) +
      ggplot2::geom_label(ggplot2::aes(x = as.Date(ypd$date[which.max(ypd$date)]), y = l, label = label), vjust = "inward", hjust = "inward", size = 3)
  }
  plt
}

#' @rdname plot_poll_of_polls
#' @export
plot_house_predictive_quantile <- function(x, y, house){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_choice(y, choices = x$y)
  checkmate::assert_subset(house, levels(x$polls_data$poll_info$.house))

  polls_bool <- houses(x$polls_data) %in% house
  pd <- subset(x$polls_data, subset = polls_bool)
  pids <- poll_ids(pd)
  ppd <- poll_predictive_distribution(x, y = y, poll_ids = pids)
  y_value <- y(pd)[, y, drop = TRUE]
  y_matrix <- matrix(rep(y_value, dim(ppd$pred_y)[1]), nrow = dim(ppd$pred_y)[1], byrow = TRUE)
  pq <- ppd$pred_y[,,y] < y_matrix
  pq <- colMeans(pq)

  dts <- collection_midpoint_dates(pd)

  ypd <- y(pd)
  ypd$date <- dts
  ypd$pq <- pq
  y(pd) <- ypd
  df <- as.data.frame(pd)

  ks <- x$known_state
  ks$x <- 0
  ks[[y]] <- 0

  plt <- ggplot2::ggplot(df, ggplot2::aes(y = pq, x = date)) +
    ggplot2::geom_point() +
    geom_known_state(ks, y)[[1]] +
    ggplot2::ylab(paste0("Pred. dist. quantile (", y, ")")) +
    ggplot2::xlab("") +
    ggplot2::ylim(0,1) +
    ggplot2::ggtitle(house[1]) +
    ggplot2::geom_hline(yintercept = 0.5, lty = "dotted") +
    ggplot2::geom_smooth(alpha = 0.3, color = "black", size = 0.5, method = 'loess', formula = y ~ x)

  plt
}


#' Plot posterior distributions of parameters
#'
#' @param x a [poll_of_polls] object
#' @param params parameters to plot (see \code{parameter_names()} too see all parameters in model)
#' @param params_plot_name names to use for parameters in plot
#' @param ... further arguments sent to bayesplot::mcmc_areas, bayesplot::mcmc_hex, etc.
#' @param title The title of the plot.
#'
#' @export
plot_parameters_areas <- function(x, params, params_plot_name = NULL, title = "Posterior distributions", ...){
  checkmate::assert_string(title)
  plot_title <- ggplot2::ggtitle(title)
  plt <- plot_parameters_bayesplot(x, bayesplot::mcmc_areas, params, params_plot_name, ...)
  plt <- plt + plot_title
  plt
}

#' @rdname plot_parameters_areas
#' @export
plot_parameters_mcmc_hex <- function(x, params, params_plot_name = NULL, title = "Posterior distributions", ...){
  checkmate::assert_string(title)
  plot_title <- ggplot2::ggtitle(title)
  plt <- plot_parameters_bayesplot(x, bayesplot::mcmc_hex, params, params_plot_name, ...)
  plt <- plt + plot_title
  plt
}

#' @rdname plot_parameters_areas
#' @export
plot_parameters_mcmc_scatter <- function(x, params, params_plot_name = NULL, title = "Posterior distributions", ...){
  checkmate::assert_string(title)
  plot_title <- ggplot2::ggtitle(title)
  plt <- plot_parameters_bayesplot(x, bayesplot::mcmc_scatter, params, params_plot_name, ...)
  plt <- plt + plot_title
  plt
}

#' Plot posterior distributions of parameters
#'
#' @param x a [poll_of_polls] object
#' @param bayeplot_FUN a plotting function from the basyeplot package
#' @param params parameters to plot (see \code{parameter_names()} too see all parameters in model)
#' @param params_plot_name names to use for parameters in plot
#' @param ... further arguments sent to bayesplot::mcmc_area
#'
plot_parameters_bayesplot <- function(x, bayeplot_FUN, params, params_plot_name = NULL, ...){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_character(params, any.missing = FALSE, unique = TRUE)
  checkmate::assert_subset(params, parameter_names(x))
  checkmate::assert_character(params_plot_name, any.missing = FALSE, unique = TRUE, len = length(params), null.ok = TRUE)

  if(is.null(params_plot_name)) params_plot_name <- params

  post <- rstan::extract(x$stan_fit, par = params[1])[[1]]
  post_matrix <- matrix(0.0, ncol = length(params), nrow = length(post), dimnames = list(NULL, params_plot_name))
  for(i in seq_along(params)){
    post_matrix[,i] <- rstan::extract(x$stan_fit, par = params[i])[[1]]
  }
  plt <- suppressWarnings(
    bayeplot_FUN(post_matrix, ...))
  plt
}


#' @rdname plot_parameters_areas
#' @importFrom graphics pairs
#' @export
pairs.poll_of_polls <- function(x, ...){
  pairs(x$stan_fit, ...)
}

#' @rdname plot_parameters_areas
#' @export
traceplot <- function(x, ...){
  UseMethod("traceplot")
}

#' @rdname plot_parameters_areas
#' @export
traceplot.poll_of_polls <- function(x, ...){
  rstan::traceplot(x$stan_fit, ...)
}
