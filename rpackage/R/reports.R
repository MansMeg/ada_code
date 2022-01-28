#' Write reports for [poll_of_polls] objects
#'
#' @param ... one or more [poll_of_polls] objects or path to rds files
#' @param report The report to generate (out of [supported_reports()])
#' @param parameters Paramaters to the individual report
#' @param output_file The file file name to write to
#' @param output_format The output format for the report
#' @param output_dir the output directory of the report
#'
#' @export
write_report <- function(..., report, parameters = NULL, output_file = NULL, output_format = 'pdf_document', output_dir = getwd()){
  # report <- "latent_states.Rmd"
  if(file.exists(report)){
    rmd_path <- report
  } else {
    checkmate::assert_choice(report, choices = supported_reports())
    rmd_path <- system.file(file.path("reports", report), package = "ada")
  }

  # Store files to use
  pops <- list(...)
  # Expand a character vector with paths
  if(inherits(pops[[1]], "character") & length(pops[[1]]) > 1L & length(pops) == 1L){
    pops <- as.list(pops[[1]])
  }
  for (i in seq_along(pops)){
    if(inherits(pops[[i]], "poll_of_polls")){
      poptfn <- tempfile(fileext = ".rds")
      saveRDS(pops[[i]], file = poptfn)
      pops[[i]] <- poptfn
    }
    checkmate::assert_file_exists(pops[[i]])
  }
  pfn <- tempfile(fileext = ".rds")
  saveRDS(parameters, file = pfn)

  # Generate rmd to render
  rmd <- readLines(rmd_path)
  rep <- paste0("c(\"", paste(unlist(pops), collapse = "\", \""), "\")")
  rmd <- gsub("\"\\[pop_paths\\]\"", rep, x = rmd)
  rmd <- gsub("\\[parameter_path\\]", pfn, x = rmd)

  rmdtfn <- tempfile(fileext = ".rmd")
  writeLines(rmd, rmdtfn)

  if(is.null(output_file)){
    output_file <- remove_file_extension(report)
  }

  rmarkdown::render(input = rmdtfn, output_format = output_format, output_file = output_file, output_dir = output_dir, knit_root_dir = getwd())

  # reports (in priority order)
  # latent states

  # Evaluation at election
  ## feed in full known state and a lot of pop objects. then sort them by latest polls
  ## elpd and percentiles at election
  ## calibration plots


  # House biases and design effects

  # Industry bias

  # Predictive distributions of polls for a model + elpd (loo)


}

#' Supported reports
#' @export
supported_reports <- function(){
  dir(system.file(file.path("reports"), package = "ada"))
}



rmd_read_pops <- function(paths){
  for (i in seq_along(paths)) {
    checkmate::assert_file_exists(paths[i])
  }
  pops <- list()
  for(i in seq_along(paths)){
    res <- try(readRDS(paths[i]), silent = TRUE)
    if(inherits(res, "try-error")) {
      warning(paths[i], " cannot be read.", call. = FALSE)
    } else {
      pops[[i]] <- res
    }
  }
  pops
}

rmd_read_params <-function(path){
  pars <-suppressWarnings(try(readRDS(path), silent = TRUE))
  if(inherits(pars, "try-error")){
    pars <- list()
  }
  pars
}

rmd_pops_unique_parties <- function(x){
  unique(unlist(lapply(x, function(X) X$y)))
}

rmd_pops_model_time_range <- function(x){
  mrts <- lapply(x, function(x) x$model_time_range)
  as.Date(c(from=min(unlist(lapply(mrts, function(z) z["from"]))),
            to=max(unlist(lapply(mrts, function(z) z["to"])))), origin = "1970-01-01")
}

rmd_pops_known_states <- function(x){
  ks <- lapply(x, function(x) x$known_state)
  ks <- do.call(rbind, ks)
  ks[!duplicated(ks),]
}

rmd_known_state_period_time_range <- function(pops){
  trs <- list()
  ks <- rmd_pops_known_states(pops)
  mtr <- rmd_pops_model_time_range(pops)
  for(i in 1:nrow(ks)){
    if(ks$date[i] >= mtr["from"] & ks$date[i] <= mtr["to"]){
      if(length(ks$date) == i) {
        ed <- unname(mtr["to"])
      } else if(ks$date[i] > mtr["to"]){
        ed <- unname(mtr["to"])
      } else {
        ed <- as.Date(paste0(lubridate::year(ks$date[i+1]), "-12-31"))
      }
      trs[[length(trs) + 1L]] <- c(from = as.Date(paste0(lubridate::year(ks$date[i]), "-01-01")),
                                   to = ed)
    }
  }
  trs
}

rmd_known_state_campaign_time_range <- function(pops, months = 6L){
  trs <- list()
  ks <- rmd_pops_known_states(pops)
  mtr <- rmd_pops_model_time_range(pops)
  for(i in 1:nrow(ks)){
    if(ks$date[i] >= mtr["from"] & ks$date[i] <= mtr["to"]){
      ed <- as.Date(paste0(lubridate::year(ks$date[i]),"-",lubridate::month(ks$date[i]) + 1L, "-01")) - lubridate::days(1)
      trs[[length(trs) + 1L]] <- c(from = as.Date(paste0(lubridate::year(ks$date[i]), "-", lubridate::month(ks$date[i]) - months, "-01")),
                                   to = ed)
    }
  }
  trs
}

rmd_model_diagnostic_table <- function(pops){
  tab <- t(do.call(rbind, lapply(pops, get_model_diagnostics)))
  colnames(tab) <- paste0("Model ", 1:ncol(tab))
  tab
}

rmd_parse_parameters <- function(parameters, defaults){
  if(is.null(parameters)){
    parameters <- list()
  } else {
    checkmate::assert_names(names(parameters),
                            subset.of = names(defaults))
  }
  # Set defaults
  for(i in seq_along(defaults)){
    if(is.null(parameters[[names(defaults)[i]]])){
      parameters[names(defaults)[i]] <- defaults[i]
    }
  }
  parameters
}

rmd_model_settings <- function(pops, unique_only = TRUE){
  for(i in seq_along(pops)) checkmate::assert_class(pops[[i]], "poll_of_polls")
  checkmate::assert_flag(unique_only)

  sha <- unlist(lapply(pops, function(x) substr(x$sha, 1,6)))
  df <- data.frame(i = 1:length(sha), sha = sha)
  df$model <- unlist(lapply(pops, function(x) x$model))
  df$y <- unlist(lapply(pops, function(x) paste(x$y, collapse = ", ")))
  df <- cbind(df, do.call(lapply(pops, function(x) as.data.frame(x$model_arguments)), what = rbind))

  df$model_time_from <- as.Date(unlist(lapply(pops, function(x) x$model_time_range["from"])), origin = "1970-01-01")
  df$model_time_to <- as.Date(unlist(lapply(pops, function(x) x$model_time_range["to"])), origin = "1970-01-01")

  df$polls_time_from <- as.Date(unlist(lapply(pops, function(x) time_range(x$polls_data)["from"])), origin = "1970-01-01")
  df$polls_time_to <- as.Date(unlist(lapply(pops, function(x) time_range(x$polls_data)["to"])), origin = "1970-01-01")

  df$known_state_time_from <- as.Date(unlist(lapply(pops, function(x) min(x$known_state$date))), origin = "1970-01-01")
  df$known_state_time_to <- as.Date(unlist(lapply(pops, function(x) max(x$known_state$date))), origin = "1970-01-01")

  gits <- unlist(lapply(pops, function(x) substr(x$git_sha, 1,6)))
  if(length(gits) == 0L) gits <- rep("", nrow(df))
  df$git_sha <- gits

  # Remove if unique
  if(unique_only) {
    is_unique <- logical(ncol(df))
    for(j in 1:ncol(df)){
      if(length(unique(df[,j])) == 1L) is_unique[j] <- TRUE
    }
    if(length(pops) > 1){
      df <- df[,!is_unique]
    } else {
      df <- df[,c("i", "sha", "model", "y", "polls_time_to")]
    }
  }
  df
}

rmd_model_settings_unique <- function(pops){
  ms <- rmd_model_settings(pops)
  rmn <- c("i", "sha", "y", "git_sha", "model_time_from", "model_time_to", "polls_time_from", "polls_time_to", "known_state_time_from", "known_state_time_to")
  mmu <- ms[,!colnames(ms) %in% rmn, drop = FALSE]
  mmu <- mmu[!duplicated(mmu),, drop = FALSE]
  mmu <- mmu[do.call(order, as.list(mmu)), ,drop = FALSE]
  mmu <- cbind(data.frame(model_setting = LETTERS[1:nrow(mmu)]), mmu)
  mmu
}

rmd_model_settings_with_unique <- function(pops){
  mm <- rmd_model_settings(pops)
  mmu <- rmd_model_settings_unique(pops)
  mmn <- merge(mm, mmu)
  mmn
}

# Return the full known state object for a party
rmd_get_known_states <- function(party){
  german_elections <- NULL
  swedish_elections <- NULL
  utils::data("german_elections", envir = environment())
  utils::data("swedish_elections", envir = environment())
  if(party %in% colnames(german_elections)) ks <- german_elections
  if(party %in% colnames(swedish_elections)) ks <- swedish_elections
  ks$date <- ks$PublDate
  ks
}

# Compute the percentiles for the last known state
rmd_last_known_state_percentile <- function(pops){
  rmd_last_known_state_evaluation(pops, type = "percentile")
}


rmd_last_known_state_evaluation <- function(pops, type){
  for(i in seq_along(pops)) checkmate::assert_class(pops[[i]], "poll_of_polls")
  checkmate::assert_choice(type, c("percentile", "rmse", "elpd"))
  results <- list()
  for(j in seq_along(pops)){
    pop <- pops[[j]]
    ls <- latent_state(pop)
    ks <- rmd_get_known_states(party = pop$y[1])
    ks <- ks[ks$date <= pop$model_time_range["to"] & ks$date >= pop$model_time_range["from"],]
    ks <- ks[which.max(ks$date),]
    if(type == "percentile"){
      results[[j]] <- as.data.frame(percentile_known_state(pop, ks))
    } else if (type == "rmse") {
      results[[j]] <- as.data.frame(rmse_known_state(x = pop, known_state = ks))
    } else if (type == "elpd") {
      results[[j]] <- as.data.frame(elpd_known_state(pop, ks))
    } else {
      stop(type, "is not implemented")
    }
    results[[j]]$sha <- substr(pop$sha,1,6)
  }
  do.call(plyr::rbind.fill, results)
}

# Feed in a party and return all other parties that exist in the known state
rmd_get_other_parties_in_known_state <- function(party){
  ks <- rmd_get_known_states(party)
  colnames(ks)[3:(which(colnames(ks) == "n") - 2L)]
}

rmd_get_kappa_name <- function(pop){
  if(grepl(x = pop$model, pattern = "model8f[0-9]+")){
    return("kappa_pred")
  } else {
    return("kappa")
  }
}

rmd_last_known_state_evaluation_by_model_setting <- function(pops, type){
  nmi <- rmd_model_settings_with_unique(pops)
  per <- rmd_last_known_state_evaluation(pops, type)
  per <- merge(per, nmi[, c("sha", "model_setting")])
  ms <- per$model_setting
  per$sha <- per$model_setting <- per$ndraws <- NULL

  res <- data.frame(model_setting = sort(unique(ms)),
                    type = 0,
                    n = 0)
  names(res)[2] <- type

  for(i in seq_along(res$model_setting)){
    msi <- res$model_setting[i]
    val <- unlist(per[ms == msi,])
    is_na <- is.na(val)
    val <- val[!is_na]
    if(type == "rmse") val <- sqrt(sum(val^2))
    if(type == "elpd") val <- sum(val)
    res[[type]][i] <- val
    res[["n"]][i] <- sum(!is_na)
  }
  res
}


rmd_print_model_information <- function(pops){
  cat("\n## Model information\n")
  if(length(pops) > 1){
    for(i in seq_along(pops)){
      cat("### Model", i, "\n")
      out <- utils::capture.output(print(pops[[i]]))
      cat("\n```",out,"\n```\n\n", sep = "\n")
    }
  } else {
    out <- utils::capture.output(print(pops[[1]]))
    cat("\n```",out,"\n```\n\n", sep = "\n")
  }
}
