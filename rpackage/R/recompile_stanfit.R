#' Recompile a poll_of_polls object
#'
#' @description
#' The function takes a [poll_of_polls] object and recompile the code an the data
#'
#' @param x a [poll_of_polls] object
#'
#' @return
#' a recompiled [stanfit] object based on the model and data of the [poll_of_polls] object
#'
#' @export
recompile_stanfit <- function(x){
  checkmate::assert_class(pop, "poll_of_polls")

  suppressWarnings(
    out <- utils::capture.output(
      stan_fit <- rstan::stan(model_code = x$stan_fit@stanmodel@model_code,
                              iter = 1, warmup = 0, chains = 1, data = x$stan_data$stan_data)
    )
  )
  stan_fit
}
