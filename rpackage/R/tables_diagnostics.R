#' Compute parameter and sampler diagnostics tables
#'
#' @param x a list with models used in report 3
#'
#' @export
table_models_parameter_diagnostics <-function(x){
  checkmate::assert_names(names(x), must.include = c("pops", "models"))
  checkmate::assert_list(x$pops, len = nrow(x$models))
  res_list <- list()
  for(i in seq_along(x$pops)){
    fit_summary <- rstan::summary(x$pops[[i]]$stan_fit)
    res <- fit_summary$summary[which(fit_summary$summary[, "Rhat"] > 1.01), c(1,3,9,10)]
    if(nrow(res) == 0) next
    res <- suppressWarnings(cbind(data.frame(parameter = rownames(res)), x$models[i,], fit_summary$summary[which(fit_summary$summary[, "Rhat"] > 1.01), c(1,3,9,10)]))
    res_list[[i]] <- res
  }
  do.call(rbind, res_list)
}


#' @rdname table_models_parameter_diagnostics
#' @export
table_models_model_diagnostics <- function(x){
  checkmate::assert_names(names(x), must.include = c("pops", "models"))
  checkmate::assert_list(x$pops, len = nrow(x$models))

  res_list <- list()
  for(i in seq_along(x$pops)){
    sampler_params <- get_sampler_params(x$pops[[i]], inc_warmup = FALSE)
    all_sampler_params <- do.call(rbind, sampler_params)
    res <- data.frame(mean_accept_stat__ = mean(all_sampler_params[, "accept_stat__"]),
                      mean_treedepth__ = mean(all_sampler_params[, "treedepth__"]),
                      max_treedepth__ = max(all_sampler_params[, "treedepth__"]),
                      mean_n_leapfrog__ = max(all_sampler_params[, "n_leapfrog__"]),
                      sum_divergent__ = sum(all_sampler_params[, "divergent__"])
    )
    res <- suppressWarnings(cbind(x$models[i,], res))
    res_list[[i]] <- res
  }
  do.call(rbind, res_list)
}




