#' Compute the elpd, percentiles and rmse for a true [known_state]
#'
#' @details
#' Incorrect draws has been removed
#'
#' @param x a [poll_of_polls] object
#' @param known_state a [known_state] object
#' @export
elpd_known_state <- function(x, known_state){
  checkmate::assert_class(x, "poll_of_polls")
  assert_known_state(known_state, null.ok = FALSE)
  if(!is.null(known_state)){
    checkmate::assert_names(x = names(known_state), must.include = c("date", x$y))
  }
  UseMethod("elpd_known_state")
}


#' @rdname elpd_known_state
#' @export
elpd_known_state.pop_model8f1 <- function(x, known_state){

  results <- matrix(0.0, nrow = length(known_state$date), ncol = length(x$y) + 1L,
                    dimnames = list(as.character(known_state$date), c(x$y, "ndraws")))

  known_state$t <- get_time_points(x, known_state$date)

  ls <- latent_state(x)
  id <- incorrect_draws(x)
  ls <- ls[!id,,]

  sigma_x <- extract(x, "sigma_x")$sigma_x[!id,]
  colnames(sigma_x) <- x$y
  sigma_xc <- extract(x, "sigma_xc")$sigma_xc
  if(!is.null(sigma_xc)) {
    colnames(sigma_xc) <- x$y
    sigma_xc <- sigma_xc[!id,]
  }

  for(i in seq_along(known_state$date)){
    for (j in seq_along(x$y)){
      if(x$stan_data$stan_data$use_latent_state_version == 0L){
        sigma <- sigma_x[,x$y[j]]
      } else if(x$stan_data$stan_data$use_latent_state_version == 3L){
        x_t_minus_1 <- ls$latent_state[,known_state$t[i] - 1, x$y[j]]

        sigma <- sqrt(x_t_minus_1 * (1 - x_t_minus_1)) * sigma_x[,x$y[j]] + sigma_xc[,x$y[j]]
      } else {
        stop("model not implemented for 'use_latent_state_version' = ",
             x$stan_data$stan_data$use_latent_state_version)
      }
      results[i, j] <- logMeanExp(dnorm(known_state[[x$y[j]]][i], mean = ls$latent_state[,known_state$t[i],x$y[j]], sd = sigma, log = TRUE))
    }
    results[i, "ndraws"] <- sum(!id)
  }
  results
}

#' @rdname elpd_known_state
#' @export
elpd_known_state.pop_model8f2 <- elpd_known_state.pop_model8f1


#' @rdname elpd_known_state
#' @export
rmse_known_state <- function(x, known_state){
  checkmate::assert_class(x, "poll_of_polls")
  assert_known_state(known_state, null.ok = FALSE)
  if(!is.null(known_state)){
    checkmate::assert_names(x = names(known_state), must.include = c("date", x$y))
  }

  results <- matrix(0.0, nrow = length(known_state$date), ncol = length(x$y) + 1L,
                    dimnames = list(as.character(known_state$date), c(x$y, "ndraws")))
  known_state$t <- get_time_points(x, known_state$date)
  ls <- latent_state(x)
  id <- incorrect_draws(x)
  ls <- ls[!id,,]

  for(i in seq_along(known_state$date)){
    for (j in seq_along(x$y)){
      results[i, x$y[j]] <- sqrt(mean((known_state[[x$y[j]]][i] - ls$latent_state[,known_state$t[i], x$y[j]])^2))
    }
    results[i, "ndraws"] <- sum(!id)
  }
  results
}

#' @rdname elpd_known_state
#' @export
percentile_known_state <- function(x, known_state){
  checkmate::assert_class(x, "poll_of_polls")
  assert_known_state(known_state, null.ok = FALSE)
  if(!is.null(known_state)){
    checkmate::assert_names(x = names(known_state), must.include = c("date", x$y))
  }

  results <- matrix(0.0, nrow = length(known_state$date), ncol = length(x$y) + 1L,
                    dimnames = list(as.character(known_state$date), c(x$y, "ndraws")))

  known_state$t <- get_time_points(x, known_state$date)
  ls <- latent_state(x)
  id <- incorrect_draws(x)
  ls <- ls[!id,,]

  for(i in seq_along(known_state$date)){
    for (j in seq_along(x$y)){
      results[i, j] <- mean(known_state[[x$y[j]]][i] > ls$latent_state[,known_state$t[i], x$y[j]])
    }
    results[i, "ndraws"] <- sum(!id)
  }

  results
}

#' What samples are correct?
#'
#' @param x a [poll_of_polls] object
#' @export
incorrect_draws <- function(x){
  checkmate::assert_class(x, "poll_of_polls")
  ls <- latent_state(x)
  incorrect_x <- apply(ls$latent_state < 0 | ls$latent_state > 1, 1, any)
  incorrect_x
}

