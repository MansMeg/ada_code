#' Compute the posterior predictive distribution of polls
#'
#' @param x a [poll_of_polls] object
#' @param ... Currently not in use.
#' @param y a vector of categories to compute the predicted distribution for
#' @param t a vector of time points to compute the predictive for for the latent state.
#' If [NULL], the whole latent state is returned.
#' If [t] exist in latent state, the latent state is returned.
#'
#' @export
x_predict <- function(x, y = NULL, t = NULL, ...){
  UseMethod("x_predict")
}

#' @rdname x_predict
#' @export
x_predict.pop_model6b <- function(x, y = NULL, t = NULL, ...){
  if(is.null(y)) y <- x$y
  if(is.null(t)) t <- x$time_line$time_line$t
  checkmate::assert_integerish(t, lower = 1)
  # t <- c(2, 3, 5, 101, 104)

  ls <- latent_state(x)
  t_max <- max(t)
  ls_max <- dim(ls$latent_state)[2]

  if(all(t <= ls_max)){
    return(ls$latent_state[,t,y])
  }
  tls_idx <- which(t <= ls_max)
  t_pred_idx <- which(t > ls_max)

  # Dynamic predictions
  predict_steps <- t_max - ls_max + 1
  x_has_ended <- x$stan_data$stan_data$t_end < ls_max

  tls_pred <- ls$latent_state[,rep(ls_max, predict_steps),y, drop = FALSE]
  dimnames(tls_pred)[2] <- list(t = ls_max:t_max)

  sigma_x_mat <- rstan::extract(x$stan_fit, "sigma_x")$sigma_x[,!x_has_ended, drop = FALSE]
  sigma_x <- array(sigma_x_mat, dim = c(dim(sigma_x_mat)[1], 1, dim(sigma_x_mat)[2]))
  for(i in (ls_max + 1):t_max){
    tls_pred[, as.character(i), !x_has_ended] <-
      rnorm(n = prod(dim(sigma_x)),
            mean = tls_pred[, as.character(i - 1), !x_has_ended, drop = FALSE],
            sd = sigma_x)
  }


  # Finalize
  tls_result <- ls$latent_state[,rep(1, length(t)), y, drop = FALSE]
  dimnames(tls_result)[2] <- list(t = t)
  if(length(tls_idx) > 0) {
    tls_result[,as.character(t[tls_idx]),y] <-
      ls$latent_state[,t[tls_idx],y,drop = FALSE]
  }
  tls_result[,as.character(t[t_pred_idx]),y] <-
    tls_pred[,as.character(t[t_pred_idx]),y,drop = FALSE]

  warning("This function does has not been tested sufficiently, please check the results.")
  return(tls_result)
}

#' @rdname x_predict
#' @export
x_predict.pop_model6c <- x_predict.pop_model6b
