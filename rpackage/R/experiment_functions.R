#' Run Simulation experiment with different settings
#'
#' @param jobs a data frame with one job per row and columns as argument
#' @param pd The [polls_data] object to use for simulated data.
#' @param values Values of interest to compute as "rmse", "elpd_nlfo" and "sigma_x|mean"
#' where the last indicates that we want the mean summary for parameter "sigma_x"
#' @param cache_folder folder with cached [poll_of_polls] objects. NULL means no cache.
#' @param figure_folder folder with cached [poll_of_polls] objects. NULL means no figures.
#' @param verbose The simulation experiment is verbose.
#' @param sim_start_date Date to start data in simulations.
#'
run_simulation_experiments <- function(jobs, pd, values = NULL, cache_folder = NULL, figure_folder = NULL, verbose = TRUE, sim_start_date = "2010-01-04"){
  checkmate::assert_data_frame(jobs)
  jobs_must_have <- c("test_idx", "ts", "mp", "npolls", "seed", "y", "model")
  jobs_can_have <- c("reweight", "rm_ptw")
  checkmate::assert_names(colnames(jobs), subset.of = c(jobs_must_have, jobs_can_have), must.include = jobs_must_have)
  checkmate::assert_class(pd, "polls_data")
  checkmate::assert_character(values, null.ok = TRUE)

  cnms <- colnames(jobs)
  vcnms <- make.names(values)

  # Add defaults
  if(! "reweight" %in% cnms) jobs$reweight <- "none"
  if(! "rm_ptw" %in% cnms) jobs$rm_ptw <- FALSE

  # Add columns
  jobs$sha <- ""
  if(!is.null(values)) jobs[, vcnms] <- numeric(1)

  # Setup folders
  if(!is.null(cache_folder)) dir.create(cache_folder, recursive = TRUE, showWarnings = FALSE)
  if(!is.null(figure_folder)) dir.create(figure_folder, recursive = TRUE, showWarnings = FALSE)

  # Load data to local environment
  e <- new.env()
  utils::data("x_test", envir = e)
  x_test <- e$x_test

  for(i in 1:nrow(jobs)){
    if(verbose && i %% 10 == 0) cat(i, "/", nrow(jobs),"\n")
    set.seed(jobs$seed[i])
    job_name <- make_job_name(jobs[,cnms], i)
    spd <- simulate_polls(x = x_test[[jobs$test_idx[i]]],
                          pd = pd,
                          npolls = jobs$npolls[i],
                          time_scale = as.character(jobs$ts[i]),
                          start_date = sim_start_date)
    spd <- reweight_and_resample(spd, true_ls = x_test[[jobs$test_idx[i]]], time_scale = as.character(jobs$ts[i]), weight = jobs$reweight[i])
    if(jobs$rm_ptw[i]) polls_time_weights(spd) <- NULL
    spd <- set_collection_period(spd, jobs$mp[i])

    pop <- poll_of_polls(y = jobs$y[i], model = jobs$model[i], polls_data = spd, time_scale = as.character(jobs$ts[i]), warmup = 4000, iter = 5000, chains = 4, cache_dir = cache_folder)
    jobs$sha[i] <- pop$sha

    # Plot Figures
    if(!is.null(figure_folder)){
      figure_fn <- file.path(figure_folder, paste0(job_name, ".png"))
      if(!file.exists(figure_fn)){
        plt <- plot(x = pop) + geom_pop_line(x = pop, y = x_test[[jobs$test_idx[i]]], color = "red") + ggplot2::theme_bw()
        ggplot2::ggsave(filename = figure_fn, width = 6, height = 3, plt)
      }
      figure_nd_fn <- file.path(figure_folder, paste0(job_name, "_nodata.png"))
      if(!file.exists(figure_nd_fn)){
        plt <- plot.poll_of_polls(x = pop, FALSE, FALSE) + geom_pop_line(x = pop, y = x_test[[jobs$test_idx[i]]], color = "red") + ggplot2::theme_bw()
        ggplot2::ggsave(filename = figure_nd_fn, width = 6, height = 3, plt)
      }
    }

    # Compute properties/results
    if("rmse" %in% values) jobs$rmse[i] <- latent_state_rmse.poll_of_polls(pop, true_ls = x_test[[jobs$test_idx[i]]], type = "full")
    if("elpd_nlfo" %in% values) jobs$elpd_nlfo[i] <- elpd_nlfo.poll_of_polls(pop, true_ls = x_test[[jobs$test_idx[i]]], t_0 = 2, type = "full", model = jobs$model[i])
    ps <- grepl(values, pattern = "\\|")
    if(any(ps)){
      psjs <- which(ps)
      for(j in seq_along(psjs)){
        tmpsplt <- strsplit(values[psjs[j]], split = "\\|")[[1]]
        pn <- tmpsplt[1]
        st <- tmpsplt[2]
        jobs[i, vcnms[psjs[j]]] <- parameter_summary(x = pop, parameter_name = pn, summary_type = st)
      }
    }
  }
  jobs
}

