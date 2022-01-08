# There are internal functions only used for experiments

#' Make a name froma a grid expand matrix
#'
#' Function to make a job name from a [grid.expand()]
#' @param x a [data.frame] from [grid.expand()]
#' @param i the row to make name for
#' @param sep char to separate argument names and values
#' @param collapse char to separate arguments
#'
make_job_name <- function(x, i, sep = "=", collapse = "|"){
  checkmate::assert_data_frame(x)
  checkmate::assert_int(i, upper = nrow(x))
  checkmate::assert_string(sep)
  checkmate::assert_string(collapse)
  jn <- paste(paste(colnames(x), as.vector(as.matrix(x[i,])), sep = sep), collapse = collapse)
}

#' @rdname make_job_name
unmake_job_name <- function(x, sep = "=", collapse = "\\|"){
  checkmate::assert_string(x)
  checkmate::assert_string(sep)
  checkmate::assert_string(collapse)
  x <- unlist(strsplit(x, split = collapse))
  y <- strsplit(x, split = sep)
  nms <- unlist(lapply(y, function(x) x[1]))
  args <- unlist(lapply(y, function(x) x[2]))
  df <- as.data.frame(matrix(args, nrow = 1, dimnames = list(NULL, nms)), stringsAsFactors = FALSE)
  for(j in 1:ncol(df)){
    num <- suppressWarnings(as.numeric(df[, j]))
    if(!any(is.na(num))){
      df[, j] <- num
    }
  }
  df
}

#' Set collection period
#'
#' @description
#' Set the collection period to test different approaches
#'
#' * [mid_collection]: Set all dates to mid of collection period
#' * [publish_date]: Set all dates to publish date
#' * [interval]: Use the collection period interval (Default)
#'
#' @param x a [polls_data] object
#' @param type type of collection period to set.
#'
set_collection_period <- function(x, type = "interval"){
  checkmate::assert_class(x, "polls_data")
  checkmate::assert_choice(type, c("interval", "publish_date", "mid_collection"))
  if(type == "interval") {
    return(x)
  } else if(type == "publish_date"){
    end_dates(x) <- publish_dates(x)
    start_dates(x) <- publish_dates(x)
    polls_time_weights(x) <- NULL
    return(x)
  } else if(type == "mid_collection"){
    mid_dates <- collection_midpoint_dates(x)
    end_dates(x) <- mid_dates
    start_dates(x) <- mid_dates
    polls_time_weights(x) <- NULL
    return(x)
  } else {
    stop("Not implemented")
  }
}


#' Compute the RMSE for the latent state
#' compared with a true latent state.
#'
#' @param x a [latent_state] object.
#' @param true_ls the true latent state
#' @param type the type of RMSE to compute [full] (a total RMSE),
#' and [series] (a series with RMSE over time)
#' @param ... further arguments (currently not used).
#'
latent_state_rmse <- function(x, true_ls, type = "series", ...){
  UseMethod("latent_state_rmse")
}

#' @rdname latent_state_rmse
latent_state_rmse.poll_of_polls <- function(x, true_ls, type = "series", ...){
  latent_state_rmse(latent_state(x), true_ls, type, ...)
}

#' @rdname latent_state_rmse
latent_state_rmse.latent_state <- function(x, true_ls, type = "series", ...){
  checkmate::assert_numeric(true_ls, len = ncol(x$latent_state))
  checkmate::assert_choice(type, choices = c("full", "series"))
  rmses <- numeric(length(true_ls))
  for(i in seq_along(true_ls)){
    rmses[i] <- sqrt(mean((x$latent_state[,i] - true_ls[i])^2))
  }
  if(type == "series"){
    return(rmses)
  } else if (type == "full"){
    return(sqrt(mean(rmses^2)))
  } else {
    stop("Not implemented")
  }
}

#' Extract a parameter summary of interest
#'
#' @param x an object to extract from
#' @param parameter_name to select (rownames of summary(stanfit))
#' @param summary_type to select (colnames of summary(stanfit))
#' @importFrom rstan summary
parameter_summary <- function(x, parameter_name, summary_type){
  checkmate::assert_class(x, "poll_of_polls")
  sfs <- summary(x$stan_fit)$summary
  checkmate::assert_choice(parameter_name, choices = row.names(sfs))
  checkmate::assert_choice(summary_type, choices = colnames(sfs))
  sfs[parameter_name, summary_type]
}




#' Compute the naive leave future out elpd
#' compared with a true latent state.
#'
#' @param x a poll_of_polls object.
#' @param true_ls the true latent state.
#' @param model Model to compute elpd_nlfo for
#' @param t_0 Starting timepoint to compute from.
#' @param type the type of elpd_nlfo to compute [full] (total elpd),
#' and [series] (a series of elpd_t)
#' @param ... further arguments (currently not used).
#'
#' @importFrom stats dnorm
#' @importFrom rstan extract
elpd_nlfo <- function(x, true_ls, model, t_0, type = "full", ...){
  UseMethod("latent_state_rmse")
}

#' @rdname elpd_nlfo
elpd_nlfo.poll_of_polls <- function(x, true_ls, model, t_0 = 2, type = "full", ...){
  checkmate::assert_numeric(true_ls, len = ncol(latent_state(x)$latent_state))
  checkmate::assert_integerish(t_0, lower = 2, upper = length(true_ls))
  checkmate::assert_choice(type, choices = c("full", "series"))
  checkmate::assert_choice(model, supported_pop_models())

  if(model %in% c("model2", "model3")){
    return(elpd_nlfo_model2(x, true_ls, t_0, type))
  } else {
    stop("elpd_nlfo not implemented for model '", model, "'.")
  }
}

elpd_nlfo_model2 <- function(x, true_ls, t_0, type){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_numeric(true_ls, len = ncol(latent_state(x)$latent_state))
  checkmate::assert_integerish(t_0, lower = 2, upper = length(true_ls))
  checkmate::assert_choice(type, choices = c("full", "series"))

  post <- extract(x$stan_fit, pars = c("x", "sigma_x"))

  elpd <- numeric(length(true_ls))
  for(t in t_0:length(true_ls)){
    elpd[t] <- log(mean(dnorm(true_ls[t], mean = post$x[,t-1], post$sigma_x)))
  }
  if(type == "series"){
    return(elpd)
  } else if (type == "full"){
    return(sum(elpd[t_0:length(elpd)]))
  } else {
    stop("Not implemented")
  }
}


#' Reweight and resample a polls_data object
#'
#' @description
#' The functions reweights and resamples a data_polls object.
#' This is used to simulated polls with different [time_weights].
#'
#' @param x a [polls_data] object.
#' @param true_ls a vector with the true latent state
#' @param time_scale The [time_scale] used for the latent state.
#' @param weight The way the collection period is weighted. See code for details.
#'
reweight_and_resample <- function(x, true_ls, time_scale, weight = "none"){
  checkmate::assert_class(x, "polls_data")
  tl <- time_line(x$time_range, time_scale)
  checkmate::assert_numeric(true_ls, len = nrow(tl$time_line))
  checkmate::assert_choice(weight, choices = c("none", "keep", "skew", "vskew"))

  if(weight == "none") return(x)

  is_long_poll_ids <- poll_ids(x)[as.integer(end_dates(x) - start_dates(x) + 1L) > 14]
  ptw <- polls_time_weights(x)
  if(weight == "skew") {
    polls_time_weights(x) <- polls_time_reweight(ptw, poll_ids = is_long_poll_ids, stats::dnbinom, size = 3, mu = 7)
  }
  if(weight == "vskew") {
    polls_time_weights(x) <- polls_time_reweight(ptw, poll_ids = is_long_poll_ids, stats::dnbinom, size = 1, mu = 4)
  }
  ydat <- sample_polls_y(x, as.data.frame(x = true_ls), tl, week_start = 1L)
  y(x) <- ydat
  x
}


#' Calibration plots for simulated data with knon state
#'
#' @param x a list of [poll_of_polls] object with the same model
#' @param true_ls a vector with the true state
#' @param alpha the alpha level for the CI
#' @param ... further arguments to [geom_histogram]
#'
calibration_plot <- function(x, true_ls, alpha = 0.05, ...){
  q <- compute_posterior_quantiles_of_true_values(x, true_ls)
  n <- 50; N <- length(q); p <- n / N
  ci <- stats::qbinom(p = c(alpha, 1-alpha), size = N, prob = p)
  df <- data.frame(q=q)
  plt <- ggplot2::ggplot(df, ggplot2::aes(x=q)) +
    ggplot2::geom_histogram(binwidth = p, boundary = 2, ...) +
    ggplot2::geom_hline(yintercept = ci[1], linetype = "dotted", color = "red") +
    ggplot2::geom_hline(yintercept = ci[2], linetype = "dotted", color = "red")
  plt

}
#' @rdname calibration_plot
calibration_qqplot <- function(x, true_ls, ...){
  q <- compute_posterior_quantiles_of_true_values(x, true_ls)
  df <- data.frame(q=q)
  ggplot2::ggplot(df) + ggplot2::geom_qq(ggplot2::aes(sample = q), distribution = stats::qunif) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red")
}

compute_posterior_quantiles_of_true_values <- function(x, true_ls){
  if(inherits(x, what = "poll_of_polls")) x <- list(x)
  for(j in seq_along(x)){
    checkmate::assert_class(x[[j]], "poll_of_polls")
  }
  checkmate::assert_numeric(true_ls, len = ncol(latent_state(x[[1]])$latent_state))

  qs <- list()
  for(j in seq_along(x)){
    post <- extract(x[[j]]$stan_fit, pars = c("x"))
    qs[[j]] <- numeric(length(true_ls))
    for(t in 1:length(true_ls)){
      qs[[j]][t] <- mean(true_ls[t] > post$x[,t])
    }
  }
  qs <- unlist(qs)
  qs
}
