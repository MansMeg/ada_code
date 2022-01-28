#' Fit a poll of polls object
#'
#' @param y character vector indicating poll variable to use
#' @param model poll of polls model to use (or a path to a stan model)
#' @param polls_data a [polls_data] object
#' @param time_scale the time scale to use, [day], [week], or [month].
#' @param model_time_range a [time_range] object that describe the period used for the latent state. Default is the [time_range] of the [polls_data] object.
#' @param known_state a [data.frame] with variables [date] and [x] indicating the known states for certain dates.
#' @param latent_time_ranges time ranges of the latent state of individual [y]s
#' @param hyper_parameters hyperparameters to supply direct to the model
#' @param slow_scales a vector of [Date]s that indicate breaks (right-inclusive) for a slower moving time scale.
#' @param ... further arguments to [rstan::stan()] function
#' @param cache_dir directory to cache model. Default is cache in tempdir(). [NULL], no cache.
#'
#' @export
poll_of_polls <- function(y,
                          model,
                          polls_data,
                          time_scale,
                          known_state = NULL,
                          model_time_range = NULL,
                          latent_time_ranges = NULL,
                          hyper_parameters = NULL,
                          slow_scales = NULL,
                          ...,
                          cache_dir = file.path(tempdir(), "pop_cache")){
  checkmate::assert_subset(x = y, choices = names(y(polls_data)))
  if(checkmate::test_file_exists(model, extension = "stan")){
    smfp <- model
    model <- remove_file_extension(basename(model))
  } else {
    smfp <- get_pop_stan_model_file_path(model)
  }
  checkmate::assert_choice(model, choices = supported_pop_models())
  assert_polls_data(polls_data, min.rows = 1, min.cols = 1)
  assert_known_state(known_state)
  if(!is.null(known_state)){
    checkmate::assert_names(x = names(known_state), must.include = c("date", y))
  }
  checkmate::assert_choice(time_scale, choices = supported_time_scales())
  if(is.null(model_time_range)) {
    mtr <- time_range(polls_data)
  } else {
    mtr <- time_range(model_time_range)
  }
  assert_poll_data_in_time_line_using_time_range(polls_data, time_scale, mtr)
  assert_latent_time_range_list(latent_time_ranges)
  ltr <- setup_latent_time_ranges(x = latent_time_ranges, y, mtr)
  assert_poll_data_and_latent_time_range_list_agree(polls_data, ltr)
  checkmate::assert_list(hyper_parameters, null.ok = TRUE)

  sd <- stan_polls_data(x = polls_data,
                        time_scale = time_scale,
                        y_name = y,
                        model = model,
                        known_state = known_state,
                        model_time_range = mtr,
                        latent_time_ranges = ltr,
                        hyper_parameters = hyper_parameters,
                        slow_scales = slow_scales)


  if(!is.null(cache_dir)) {
    if(!checkmate::test_directory(cache_dir)) dir.create(cache_dir)
    checkmate::assert_directory(cache_dir)
  }

  # SHA is setup both of all
  fun_args <- names(formals(ada::poll_of_polls))[-which(names(formals(poll_of_polls)) %in% c("...", "cache_dir"))]
  sha_fun_args <- list(y = y,
                       model = readLines(smfp),
                       polls_data = polls_data,
                       time_scale = time_scale,
                       known_state = known_state,
                       model_time_range = mtr,
                       latent_time_ranges = ltr,
                       hyper_parameters = hyper_parameters,
                       slow_scales = slow_scales)
  checkmate::assert_set_equal(fun_args, names(sha_fun_args))
  sha_stan_args <- list(data = sd$stan_data, ...)
  sha <- digest::digest(c(sha_fun_args, sha_stan_args), algo = "sha1")

  # Read from cache
  if(!is.null(cache_dir)){
    cache_fp <- cache_file_path(sha, cache_dir)
    if(file.exists(cache_fp)){
      pop <- readRDS(file = cache_fp)
      message("Cached results used.")
      return(pop)
    } else {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Setup rstan arguments
  rstan_arguments <- list(...)
  if(!is.null(rstan_arguments$data)) warning("The 'data' argument has been overwritten")
  rstan_arguments$data <- sd$stan_data
  if(is.null(rstan_arguments$file)) rstan_arguments$file <- smfp
  if(is.null(rstan_arguments$model_name)) rstan_arguments$model_name <- model
  if(is.null(rstan_arguments$pars)) rstan_arguments$pars <- stan_parameters_to_store(model)

  # Run Stan
  stan_fit <- do.call(rstan::stan, rstan_arguments)

  pop <-  list(y = y,
               model = model,
               polls_data = polls_data,
               time_scale = time_scale,
               known_state = known_state,
               model_time_range = mtr,
               latent_time_range = ltr,
               stan_arguments = list(...),
               sha = sha,
               git_sha = get_git_sha(),
               cache_dir = cache_dir,
               time_line = sd$time_line,
               stan_fit = stan_fit,
               diagnostics = compute_diagnostics(stan_fit),
               model_arguments = hyper_parameters,
               stan_data = sd)
  class(pop) <- c(paste0("pop_", model), "poll_of_polls")

  assert_pop(pop)
  # Save to cache
  if(!is.null(cache_dir)) saveRDS(pop, file = cache_fp)
  pop
}

assert_pop <- function(x){
  checkmate::assert_class(x, "poll_of_polls")
}

assert_known_state <- function(x, null.ok = TRUE){
  if(is.null(x)) {
    if(null.ok){
      return(invisible(TRUE))
    } else {
      stop("known_state is NULL", call. = FALSE)
    }
  }
  checkmate::assert_data_frame(x, min.rows = 1)
  checkmate::assert_names(colnames(x), must.include = c("date"))
}

cache_file_path <- function(sha, cache_dir){
  file.path(cache_dir, paste0(sha, ".rds"))
}


#' Parameters we want to store, by model
#'
#' @details
#' The function returns the parameter names of the parameters that are needed
#' for analysis or in computing predictive distributions.
#' Hence these parameters will be the only ones that will be stored
#' when running Stan.
#'
#' @param model a model name to get the parameters to store from.
#'
#' @keywords internal
stan_parameters_to_store <- function(model){
  checkmate::assert_choice(model, choices = supported_pop_models())
  if(model %in% c("model2", "model3", "model5", "model6", "model6b", "model6c")){
    return(c("x", "sigma_x", "lp__"))
  } else if(model %in% c("model4")){
      return(c("x", "sigma_x", "nu", "lp__"))
  } else if(model %in% c("model7")){
    return(c("x", "sigma_x", "lp__","V_known","y_pred","nu"))
  } else if(model %in% c("model8a", "model8a1", "model8a3", "model8a4")){
    return(c("x", "sigma_x", "lp__", "kappa", "sigma_kappa"))
  } else if(model %in% c("model8b", "model8b1")){
    return(c("x", "sigma_x", "lp__", "beta_mu", "sigma_beta_mu"))
  } else if(model %in% c("model8c", "model8c2")){
    return(c("x", "sigma_x", "lp__", "beta_sigma", "sigma_beta_sigma"))
  } else if(substr(model,1,7) %in% c("model8d")){
    return(c("x", "sigma_x", "lp__", "kappa", "sigma_kappa", "beta_mu", "sigma_beta_mu", "beta_sigma", "sigma_beta_sigma"))
  } else if(substr(model,1,7) %in% c("model8e")){
    return(c("x", "sigma_x", "lp__", "kappa", "sigma_kappa", "beta_mu", "sigma_beta_mu", "beta_sigma", "sigma_beta_sigma", "alpha_kappa", "alpha_beta_mu", "alpha_beta_sigma", "sigma_xc"))
  } else if(model %in% c("model8f")){
    return(c("x", "x_pred", "sigma_x", "lp__", "kappa", "sigma_kappa", "beta_mu", "sigma_beta_mu", "beta_sigma", "sigma_beta_sigma", "alpha_kappa", "alpha_beta_mu", "alpha_beta_sigma", "sigma_xc", "kappa_next_pred"))
  } else if(model %in% c("model8f1")){
    return(c("x_pred", "sigma_x", "lp__", "kappa_pred", "sigma_kappa", "beta_mu", "sigma_beta_mu", "beta_sigma", "sigma_beta_sigma", "alpha_kappa", "alpha_beta_mu", "alpha_beta_sigma", "sigma_xc"))
  } else if(model %in% c("model8f2")){
    return(c("x_pred", "sigma_x", "lp__", "kappa_pred", "sigma_kappa", "beta_mu", "sigma_beta_mu", "beta_sigma", "sigma_beta_sigma", "alpha_kappa", "alpha_beta_mu", "alpha_beta_sigma", "sigma_xc", "nu_kappa", "v_kappa"))
  } else if(model %in% c("model8f3")){
    return(c("x_pred", "sigma_x", "lp__",
             "kappa_pred", "sigma_kappa", "sigma_xc",
             "beta_mu", "sigma_beta_mu",
             "beta_sigma", "sigma_beta_sigma",
             "alpha_kappa", "alpha_beta_mu", "alpha_beta_sigma",
             "nu_kappa", "v_kappa",
             "alpha_V","theta_x","ar_V", "V"))
  } else if(model %in% c("model8g")){
    return(c("x_pred", "sigma_x", "lp__",
             "Omega",
             "kappa_pred", "sigma_kappa", "sigma_xc",
             "beta_mu", "sigma_beta_mu",
             "beta_sigma", "sigma_beta_sigma",
             "alpha_kappa", "alpha_beta_mu", "alpha_beta_sigma",
             "nu_kappa", "v_kappa",
             "alpha_V","theta_x","ar_V", "V"))
  } else if(model %in% c("model8g1", "model8g2", "model8g3", "model8h2", "model8h3",  "model8h4")){
    return(c("x_pred", "sigma_x", "lp__", "x", "eta",
             "Omega",
             "kappa_pred", "sigma_kappa", "sigma_xc",
             "beta_mu", "sigma_beta_mu",
             "beta_sigma", "sigma_beta_sigma",
             "alpha_kappa", "alpha_beta_mu", "alpha_beta_sigma",
             "nu_kappa", "v_kappa",
             "alpha_V","theta_x","ar_V", "V"))
  } else if(grepl(model, pattern = "^model8i[0-9]+$")){
    return(c("x_pred", "sigma_x", "lp__", "eta",
             "kappa_pred", "sigma_kappa", "sigma_xc",
             "beta_mu", "sigma_beta_mu",
             "beta_sigma", "sigma_beta_sigma",
             "alpha_kappa", "alpha_beta_mu", "alpha_beta_sigma",
             "nu_kappa", "v_kappa",
             "psi",
             "alpha_V","theta_x","ar_V", "V"))
  } else if(model %in% c("model9")){
    return(c("x", "lp__", "Omega_z","Sigma_Z"))
  } else if(model %in% c("model10d")){
    return(c("x","V", "theta_x", "sigma_x", "lp__", "kappa", "sigma_kappa", "beta_mu", "sigma_beta_mu", "beta_sigma", "sigma_beta_sigma"))
  }else if(model %in% c("model10e")){
    return(c("alpha_V","theta_x","ar_V","ar_x","x","V", "sigma_x", "lp__", "kappa", "sigma_kappa", "beta_mu", "sigma_beta_mu", "beta_sigma", "sigma_beta_sigma"))
  }else if(model %in% c("model11a")){
    return(c("tau_x","Omega_x","x", "lp__", "kappa", "sigma_kappa", "beta_mu", "sigma_beta_mu", "beta_sigma", "sigma_beta_sigma"))
  }else if(model %in% c("model11b")){
    return(c("tau_x","Omega_x","x", "lp__", "kappa", "sigma_kappa",
             "beta_mu", "sigma_beta_mu", "beta_sigma", "sigma_beta_sigma",
             "alpha_kappa", "alpha_beta_mu", "alpha_beta_sigma", "sigma_xc"))
  }else {
    stop("'", model, "' not implemented in stan_parameters_to_store().")
  }
}

#' Parameters that are not state parameters, by model
#'
#' @details
#' The function returns the parameter names of the parameters
#' that are not state parameters and hence can be visualized.
#'
#' @param model a model name to get non-state parameters from.
#'
#' @keywords internal
stan_non_state_parameters <- function(model){
  checkmate::assert_choice(model, choices = supported_pop_models())
  if(model %in% c("model2", "model3", "model5", "model6", "model6b", "model6c")){
    return(c("sigma_x"))
  } else if(model %in% c("model7")){
    return(c("sigma_x", "V_known","y_pred","nu"))
  } else if(model %in% c("model4")){
      return(c("sigma_x", "nu"))
  } else if(model %in% c("model8a", "model8a1", "model8a3", "model8a4")){
    return(c("sigma_x", "kappa", "sigma_kappa"))
  } else if(model %in% c("model8b", "model8b1")){
    return(c("sigma_x", "beta_mu", "sigma_beta_mu"))
  } else if(model %in% c("model8c")){
    return(c("sigma_x", "beta_sigma", "sigma_beta_sigma"))
  } else {
    stop("'", model, "' not implemented in stan_non_state_parameters().")
  }
}

supported_pop_models <- function() {
  # When updating a model, the following parts needs to also be fixed:
  # a. Update stan_polls_data() with how the input data looks like
  # b. Update stan_parameters_to_store() with info on what parameters should be stored and used
  # c. Update stan_non_state_parameters() with info on what parameters should be stored and used
  # d. Update latent_state.stanfit() with info on how the latent state is extracted
  # e. Update compute_prediction_error()
  c("model2", "model3", "model4", "model5",
    "model6", "model6b",  "model6c",
    "model7",
    "model8a", "model8a1", "model8a3", "model8a4",
    "model8b", "model8b1",
    "model8c", "model8c2",
    "model8d", "model8d2", "model8d3",
    "model8e", "model8e2",
    "model8f", "model8f1", "model8f2", "model8f3",
    "model8g", "model8g1", "model8g2", "model8g3",
    "model8h2", "model8h3", "model8h4",
    paste0("model8i", 1:1),
    "model9",
    "model10d", "model10e",
    "model11a", "model11b")
}

get_pop_stan_model_file_path <-function(model){
  checkmate::assert_choice(model, supported_pop_models())
  fp <- file.path(system.file(package = "ada"), "stan_models", paste0(model, ".stan"))
  checkmate::assert_file_exists(fp)
  fp
}

#' @export
print.poll_of_polls <- function(x, ...){
  ms <- utils::capture.output(print(utils::object.size(x), units = "auto", standard = "SI"))
  cat("==== Poll of Polls Model (",ms,") ==== \n", sep = "")
  tr <- time_range(x$time_line)
  cat("Model is fit during the period ", as.character(tr["from"]), "--", as.character(tr["to"]), "\n", sep = "")
  cat("Stan model: ", x$model, ".stan\n", sep = "")
  cat("Number of parameters:", suppressWarnings(length(parameter_names(x))), "\n")
  cat("Parties:", paste0(x$y, collapse = ", "), "\n")
  cat("Time scale:", x$time_scale, "\n")


  cat("\n== Data == \n")
  polls_data_summary(x$polls_data)
  cat("\n")
  known_state_summary(x$known_state)

  cat("\n== Stan arguments == \n")
  cat(yaml::as.yaml(x$stan_arguments))

  cat("\n== Model arguments == \n")
  cat(yaml::as.yaml(x$model_arguments))

  cat("\n== Model diagnostics == \n")
  out <- utils::capture.output(md <- try(get_model_diagnostics(x), silent = TRUE))
  if(inherits(md, what = "try-error")){
    cat(out, "\n")
  } else {
    # Round digits to zero (and make ints)
    zero <- c("no_divergent_transistions", "no_max_treedepth", "no_low_bfmi_chains", "no_Rhat_above_1_1", "no_Rhat_is_NA", "mean_chain_warmup_time", "mean_chain_sampling_time")
    md[zero] <- lapply(lapply(md[zero], round), as.integer)
    # Round digits to one (and make ints)
    one <- c("mean_no_leapfrog_steps")
    md[one] <- lapply(lapply(md[one], round, digits = 1), as.integer)
    cat(yaml::as.yaml(md))
  }

  if(!is.null(x$git_sha)){
    cat("\n== Git == \n")
    cat("git sha:", x$git_sha, "\n")
  }

  if(!is.null(x$cache_dir)){
    cat("\n== Cache == \n")
    cat("sha:", x$sha, "\n")
    cat("cache directory:", x$cache_dir, "\n")
  }

}



assert_poll_data_and_latent_time_range_list_agree <- function(x, ltr){
  checkmate::assert_class(x, "polls_data")
  assert_latent_time_ranges(ltr)
  ys <- y(x)
  sd <- start_date(x)
  ed <- end_date(x)
  yn <- names(ltr)
  for(i in seq_along(ltr)){
    pre_polls <- ltr[[yn[i]]]["from"] > sd
    if(any(pre_polls)){
      value <- ys[[yn[i]]][pre_polls]
      is_incorrect <- !(is.na(value) | value == 0)
      if(any(is_incorrect)){
        pre_polls[pre_polls] <- is_incorrect
        stop("Polls has values for category '", yn[i],
             "' that should be NA before '",
             ltr[[yn[i]]]["from"], "':\n",
             paste0(poll_ids(x)[pre_polls], collapse = ", "), call. = FALSE)
      }
    }
    post_polls <- ltr[[yn[i]]]["to"] < ed
    if(any(post_polls)){
      value <- ys[[yn[i]]][post_polls]
      is_incorrect <- !(is.na(value) | value == 0)
      if(any(is_incorrect)){
        post_polls[post_polls] <- is_incorrect
        stop("Polls has values for category '", yn[i],
             "' that should be NA after '",
             ltr[[yn[i]]]["to"], "':\n",
             paste0(poll_ids(x)[post_polls], collapse = ", "), call. = FALSE)
      }
    }
  }
}


#' Extract the parameter names and parameter counts from a poll_of_polls object
#'
#' @param x a [poll_of_polls] object
#' @export
parameter_names <- function(x){
  res <- try(names(x$stan_fit), silent = TRUE)
  if(inherits(res, "try-error")){
    warning("Stan model does not contain samples.")
    return(NULL)
  }
  res
}

#' @rdname parameter_names
#' @export
parameter_block_names <- function(x){
  parameters_names_remove_indecies(parameter_names(x))
}

parameters_names_remove_indecies <- function(nms){
  checkmate::assert_character(nms)
  nms <- sub("\\[[0-9]+\\]", "", nms)
  nms <- sub("\\[[0-9]+,[0-9]+\\]", "", nms)
  nms <- sub("\\[[0-9]+,[0-9]+,[0-9]+\\]", "", nms)
  nms
}

#' @rdname parameter_names
#' @export
parameters_counts <- function(x){
  table(parameter_block_names(x))
}



#' @rdname parameter_names
#' @export
get_parameter_names <- parameter_names


#' Get the model arguments used in a [poll_of_polls] object
#'
#' @param x a [poll_of_polls] object
#' @param all Should all model arguments be returned, or just the one that were set?
#'
#' @return a list with model arguments
#'
#' @export
get_model_model_arguments <- function(x, all = TRUE){
  checkmate::assert_class(x, classes = "poll_of_polls")
  if(all){
    ma <- x$stan_data$stan_data[model_arguments(x$model)]
    ma <- ma[!is.na(names(ma))]
  } else {
    ma <- x$model_arguments
  }
  ma
}


#' Get the model diagnostics used in a [poll_of_polls] object
#'
#' @param x a [poll_of_polls] object
#'
#' @details
#' no_divergent_transitions: The number of draws with divergent transitions
#' no_max_treedepth: The number of draws where the max treedepth was reached
#' no_low_bfmi_chains: The number of chains with low E-BFMI
#' no_Rhat_above_1_1: The number of parameters with an Rhat above 1.1
#' no_Rhat_is_NA: The number of parameters without an Rhat value
#' mean_no_leapfrog_steps: The mean number of leapfrog steps taken per iteration/draw
#' mean_chain_step_size: The mean stepsize over the chains
#' mean_chain_inv_mass_matrix_min_max: The mean max and min of the invers mass matrix over the chains
#' mean_chain_warmup_time: the mean warmup time in seconds
#' mean_chain_sampling_time: the mean warmup time in seconds
#'
#' @return a list with diagnostic values
#'
#' @export
get_model_diagnostics <- function(x){
  checkmate::assert_class(x, "poll_of_polls")
  res <- list()
  res$no_divergent_transistions <- sum(rstan::get_divergent_iterations(x$stan_fit))
  res$no_max_treedepth <- sum(rstan::get_max_treedepth_iterations(x$stan_fit))
  res$no_low_bfmi_chains <- length(rstan::get_low_bfmi_chains(x$stan_fit))
  if(!is.null(x$diagnostics)){
    res$no_Rhat_above_1_1 <- sum(x$diagnostics$Rhat[!is.na(x$diagnostics$Rhat)] > 1.1)
    res$no_Rhat_is_NA <- sum(is.na(x$diagnostics$Rhat))
  }

  res$mean_no_leapfrog_steps <- mean(rstan::get_num_leapfrog_per_iteration(x$stan_fit))
  ai <- get_adaptation_info(x)
  res$mean_chain_step_size <- mean(unlist(lapply(ai, function(x) x$step_size)))
  res$mean_chain_inv_mass_matrix_min <-
    mean(unlist(lapply(ai, function(x) min(x$diag_inv_mass_matrix))))
  res$mean_chain_inv_mass_matrix_max <-
    mean(unlist(lapply(ai, function(x) max(x$diag_inv_mass_matrix))))

  tm <- rstan::get_elapsed_time(x$stan_fit)
  res$mean_chain_warmup_time <- mean(tm[,"warmup"])
  res$mean_chain_sampling_time <- mean(tm[,"sample"])
  res
}

#' Get the number of draws/samples in a [poll_of_polls] object
#'
#' @param x  a [poll_of_polls] object
#' @export
get_ndraws <- function(x){
  checkmate::assert_class(x, "poll_of_polls")
  sum(unlist(lapply(x$stan_fit@stan_args, function(x) {x$iter - x$warmup})))
}

#' @rdname get_ndraws
#' @export
ndraws <- get_ndraws


get_git_sha <- function(){
  gsha <- try(git2r::revparse_single(git2r::repository(),"HEAD")$sha, silent = TRUE)
  if(inherits(gsha, "try-error")) gsha <- NULL
  gsha
}


compute_diagnostics <- function(x){
  fit_summary <- rstan::summary(x)
  list(n_eff = fit_summary$summary[,"n_eff"],
       Rhat = fit_summary$summary[,"Rhat"])
}
