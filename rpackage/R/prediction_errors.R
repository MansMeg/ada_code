#' Compute prediction error for a poll_of_polls model
#'
#' @param x a [poll_of_polls] model
#' @param predict_state the true value to predict
#' @param type error measurement to compute
#'
#' @export
compute_prediction_error <- function(x, predict_state, type = "elpd"){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_data_frame(predict_state, nrows = 1)
  checkmate::assert_names(names(predict_state), must.include = c(x$y, "date"))
  checkmate::assert_true(predict_state$date %in% x$time_line$daily$date)
  checkmate::assert_choice(type, choices = c("elpd", "rmse", "quantile"))
  if(x$model == "model5"){
    compute_prediction_error_model5(x, predict_state, type)
  } else if(x$model %in% c("model6", "model6b", "model6c")){
    compute_prediction_error_model6(x, predict_state, type)
  } else {
    stop("Model prediction errors for '", x$model, "' is not implemented.")
  }
}

compute_prediction_error_model5 <- function(x, predict_state, type = "elpd"){
  # Identify state t to predict
  predict_t <- x$time_line$daily$time_line_t[x$time_line$daily$date %in%  predict_state$date]

  lsv <- paste0("x[", predict_t, "]")
  post <- rstan::extract(x$stan_fit, pars = c(lsv, "sigma_x"))

  if(type == "elpd"){
    res <- log(mean(dnorm(predict_state$x, mean = post[[lsv]], post$sigma_x)))
  } else if(type == "rmse"){
    res <- sqrt(mean((predict_state$x - post[[lsv]])^2))
  } else if(type == "quantile"){
    res <- mean(predict_state$x < post[[lsv]])
  } else {
    stop("not implemented")
  }
  return(res)
}

compute_prediction_error_model6 <- function(x, predict_state, type = "elpd"){
  # Identify state t to predict
  predict_t <- x$time_line$daily$time_line_t[x$time_line$daily$date %in%  predict_state$date]

  ls <- latent_state(x)
  if(type == "elpd"){
    post <- rstan::extract(x$stan_fit, pars = "sigma_x")
    colnames(post$sigma_x) <- x$y
  }
  pe <- numeric(length(x$y))
  names(pe) <- x$y
  for(i in seq_along(pe)){
    if(type == "elpd"){
      pe[i] <- log(mean(dnorm(predict_state[[x$y[i]]], mean = ls$latent_state[,predict_t,x$y[i]], post$sigma_x[,x$y[i]])))
    } else if(type == "rmse"){
      pe[i] <- sqrt(mean((predict_state[[x$y[i]]] - ls$latent_state[,predict_t,x$y[i]])^2))
    } else if(type == "quantile"){
      pe[i] <- mean(predict_state[[x$y[i]]] > ls$latent_state[,predict_t,x$y[i]])
    } else {
      stop("not implemented")
    }
  }
  return(pe)
}
