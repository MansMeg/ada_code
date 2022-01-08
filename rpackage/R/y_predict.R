#' Compute the posterior predictive distribution of polls
#'
#' @param x a [poll_of_polls] object
#' @param y a vector of categories to compute the predicted distribution for
#' @param poll_ids a vector of poll ids to compute the predictive for. If [NULL] all are computed.
#' @param ... Currently not used.
#'
#' @export
y_predict <- function(x, y = NULL, poll_ids = NULL, ...){
  UseMethod("y_predict")
}

#' @rdname y_predict
#' @export
poll_predictive_distribution <- y_predict

#' @rdname y_predict
#' @export
y_predict.pop_model6b <- function(x, y = NULL, poll_ids = NULL, ...){
  if(is.null(y)) y <- x$y
  if(is.null(poll_ids)) poll_ids <- poll_ids(x$polls_data)

  ls <- latent_state(x)
  ptw <- polls_time_weights(x$stan_data)
  ptw$t <- get_time_points_from_time_line(ptw$date, tl = x$stan_data$time_line)
  ptw <- dplyr::left_join(ptw, x$stan_data$poll_ids, by = ".poll_id")
  ptw <- ptw[ptw$.poll_id %in% poll_ids,]

  dims <- dim(ls$latent_state)

  # Compute mu
  mu <- array(0.0,
              dim = c(dim(ls$latent_state)[1], length(poll_ids), length(y)),
              dimnames = list(iterations = NULL, poll_ids = poll_ids, categories = y))
  for(l in 1:nrow(ptw)){
    mu[,ptw$.poll_id[l],y] <- mu[,ptw$.poll_id[l],y] + ptw$weight[l] * ls$latent_state[,ptw$t[l],y]
  }

  # Compute sigma
  sigma_y <- as.data.frame(x$stan_data$stan_data$sigma_y)
  sigma_y$i <- 1:nrow(sigma_y)
  sigma_y <- dplyr::left_join(sigma_y, x$stan_data$poll_ids, by = "i")
  sigma_y <- sigma_y[which(sigma_y$.poll_id %in% poll_ids),y]
  sigma_y <- as.matrix(sigma_y)
  sigma_y <- array(rep(sigma_y, dim(mu)[1]),dim = c(dim(sigma_y)[1],dim(sigma_y)[2],dim(mu)[1]))
  sigma_y <- aperm(sigma_y, c(3,1,2))
  dimnames(sigma_y) <- dimnames(mu)

  # Compute predictive
  pred_y <- array(rnorm(n = prod(dim(mu)), mean = mu, sd = sigma_y), dim = dim(mu))
  dimnames(pred_y) <- dimnames(mu)


  return(list(mu = mu, sigma_y = sigma_y, pred_y = pred_y))
}

#' @rdname y_predict
#' @export
y_predict.pop_model6c <- y_predict.pop_model6b
