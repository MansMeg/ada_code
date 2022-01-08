#' Generate sbatch scripts to run all files in a folder
#'
#' @param directory a directory with files to submit with batch
#'
#' @export
slurm_model_runs <- function(directory){
  cat(paste0("sbatch ", dir(directory, full.names = TRUE)), sep = "\n")
}

#' @rdname slurm_model_runs
#' @export
generate_sbatch_from_path <- slurm_model_runs

#' @rdname slurm_model_runs
#' @export
bash_script_sbatch_files  <- slurm_model_runs

#' @rdname slurm_model_runs
#' @export
triton_model_runs <- function(directory = "triton/model_runs"){
  slurm_model_runs(directory)
}



#' Get the periods to use in modeling
#'
#' @param cfg a cfg
#' @param known_state a known state_object
#' @export
get_model_periods <- function(cfg, known_state){
  checkmate::assert_list(cfg)
  checkmate::assert_list(cfg$model_arguments)
  checkmate::assert_data_frame(known_state, null.ok = TRUE)
  if(!is.null(known_state)){
    checkmate::assert_date(known_state$date)
  }

  mp <- data.frame(model_start_date = as.Date(cfg$model_arguments$date$start),
                   model_end_date = as.Date(cfg$model_arguments$date$end),
                   polls_start_date = as.Date(cfg$model_arguments$date$start),
                   polls_end_date = as.Date(cfg$model_arguments$date$end),
                   known_state_start_date = as.Date(cfg$model_arguments$date$start),
                   known_state_end_date = as.Date(cfg$model_arguments$date$end))

  if(!is.null(cfg$model_arguments$predict_known_state) &
     !is.null(known_state$date)){
    checkmate::assert_integerish(cfg$model_arguments$predict_known_state, lower = 0)
    if(is.null(cfg$model_arguments$min_known_states)) cfg$model_arguments$min_known_states <- 0
    checkmate::assert_integerish(cfg$model_arguments$min_known_states, lower = 0, null.ok = FALSE)
    ks_to_use <- order(known_state$date) > cfg$model_arguments$min_known_states
    g <- expand.grid(known_state_date = known_state$date[ks_to_use],
                     predict_days = cfg$model_arguments$predict_known_state)
    for(i in 1:nrow(g)){
      mt <- data.frame(model_start_date = as.Date(cfg$model_arguments$date$start),
                       model_end_date = as.Date(g$known_state_date[i]),
                       polls_start_date = as.Date(cfg$model_arguments$date$start),
                       polls_end_date = as.Date(g$known_state_date[i]) - lubridate::days(g$predict_days[i]),
                       known_state_start_date = as.Date(cfg$model_arguments$date$start),
                       known_state_end_date = as.Date(g$known_state_date[i]) - lubridate::days(1))
      mp <- rbind(mp, mt)
    }
  }

  mp$model_no <- 1:nrow(mp)
  mp <- mp[,c(ncol(mp), 1:(ncol(mp)-1))]
  return(mp)
}


#' Load models from model output directory
#'
#' @param dir directury with model files
#' @export
load_models <- function(dir){
  checkmate::assert_directory(dir)
  fps <- dir(dir, full.names = TRUE)

  # Reorder according to model number
  ord <- stringr::str_extract_all(string = fps, "model_[0-9]+")
  ord <- as.numeric(unlist(lapply(ord, function(x) stringr::str_extract(x[length(x)], "[0-9]+"))))
  fps <- fps[order(ord)]

  m <- list()
  for(i in seq_along(fps)){
    e <- new.env()
    load(file = fps[i], envir = e)
    e <- as.list(e)
    if(i == 1L){
      m <- e
      m$pops <- list(e$pop)
      m$pop <- NULL
    } else {
      m$pops[[i]] <- e$pop
    }
  }
  m
}



#' Return parties existing in a given model range
#'
#' @param y a character vector of party names
#' @param ltr the [time_range] of the latent series
#' @param mtr the [time_range] of the model
#'
#' @export
existing_parties <- function(y, ltr, mtr){
  assert_latent_time_range_list(ltr)
  in_mtr <- !logical(length(y))
  for(i in seq_along(y)){
    if(is.null(ltr[[y[i]]])) next
    tests <- c(mtr["from"] > ltr[[y[i]]]$to, mtr["to"] < ltr[[y[i]]]$from)
    if(any(tests)) {
      in_mtr[i] <- FALSE
      message("Category '", y[i], "' is exluded. The latent time range is not included in the model time range (", paste0(mtr, collapse = "--"), ").")
    }
  }
  y[in_mtr]
}

#' Generate a script to remove all files in a folder
#'
#' @param directory the directory with files to remove
#' @param ... further arguments to [dir]
#'
#' @export
bash_script_remove_files <- function(directory, ...){
  cat(paste0("rm ", dir(directory, ...)), sep = "\n")
}

