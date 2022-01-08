#' Write poll of polls models
#'
#' @param x a [poll_of_polls] object to write
#' @param file the file path
#' @param file_name the output file name.
#' @param output_dir output directory
#' @param draw_idx draw index to write.
#' @param percentiles percentiles to output
#' @param aggregated_series a list with combinations of aggregations to make (e.g. S, V and MP). Default is no aggregations.
#' @param last_no_t last number of time points to include in output
#'
#' @export
write_latent_mean_csv <- function(x, file){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_path_for_output(file)
  lsm <- latent_state_mean(x)
  utils::write.csv2(lsm, file, row.names = FALSE)
}

#' @rdname write_latent_mean_csv
#' @export
write_latent_draw_csv <- function(x, file, draw_idx){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_path_for_output(file)
  checkmate::assert_integerish(draw_idx, ndraws(x))
  lsd <- latent_state_draw(x, draw_idx)
  utils::write.csv2(lsd, file, row.names = FALSE)
}

#' @rdname write_latent_mean_csv
#' @export
write_ada_json <- function(x, output_dir = getwd(), file_name = NULL, percentiles = c(0.05,0.125,0.5,0.875,0.95)){
  .Deprecated("write_ada_time_series_json")
  write_ada_time_series_json(x, output_dir, file_name, percentiles)
}

#' @rdname write_latent_mean_csv
#' @export
write_ada_time_series_json <- function(x, output_dir = getwd(), file_name = NULL, percentiles = c(0.05,0.125,0.5,0.875,0.95), aggregated_series = list()){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_directory_exists(output_dir)
  checkmate::assert_numeric(percentiles, lower = 0, upper = 1, sorted = TRUE, unique = TRUE)
  checkmate::assert_list(aggregated_series)

  metadata <- ada_metadata(x)
  parameters <- list(stan_arguments = x$stan_arguments)

  elections <- x$known_state[, c("date", x$y)]
  elections <- as.list(elections)

  polls <- dplyr::left_join(x$polls_data$poll_info, x$polls_data$y[,c(".poll_id", x$y)])
  names(polls) <- gsub("\\.", "", names(polls))
  polls <- as.list(polls)

  ls <- latent_state(x)
  for(i in seq_along(aggregated_series)){
    lss <- latent_state_subset(ls, categories = aggregated_series[[i]])
    lss <- latent_state_apply(x = lss, MARGIN = c(1,2), FUN = sum)
    dimnames(lss$latent_state)$categories <- paste(aggregated_series[[i]], collapse = "_")
    ls <- latent_state_abind(ls, lss, along = 3L)
  }
  lsp <- latent_state_percentiles(ls, percentiles)
  names(lsp)[1:length(percentiles)] <-
    percentile_col_names(x = names(lsp)[1:length(percentiles)])
  names(lsp)[length(percentiles)+1] <- "party"
  lsp_full <- lsp

  party <- lsp$party
  lsp$party <- NULL
  lsp$date <- NULL
  lsp$t <- NULL
  lsp2 <- split(lsp, f = party)
  lsp2_full <- split(lsp_full, f = party)
  lsp2 <- lapply(lsp2, as.list)
  lsp3 <- c(list(dates=lsp2_full[[1]]$date),
    list(t=lsp2_full[[1]]$t),
    lsp2)

  out <- list(metadata = metadata,
              parameters = parameters,
              elections = elections,
              polls = polls,
              latent_state = lsp3)

  if(is.null(file_name)){
    file_name <- paste0("adapop_ts_", as.Date(out$metadata$written), "_", substr(out$metadata$object_sha,1,6), ".json")
  }
  fp <- file.path(output_dir, file_name)
  jsonlite::write_json(out, path = fp, auto_unbox = TRUE, pretty = TRUE, digits = 4)
}

#' @rdname write_latent_mean_csv
#' @export
write_ada_latent_posterior_json <- function(x, output_dir = getwd(), file_name = NULL, last_no_t  = NULL){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_directory_exists(output_dir)
  checkmate::assert_integerish(last_no_t, lower = 1, upper = max(x$time_line$time_line$t), null.ok = TRUE)

  metadata <- ada_metadata(x)
  parameters <- list(stan_arguments = x$stan_arguments)

  xls <- latent_state(x)
  dimnames(xls$latent_state)[[2]] <- as.character(xls$time_line$time_line$date)
  if(!is.null(last_no_t)){
    no_t <- dim(xls$latent_state)[2]
    xls$latent_state <- xls$latent_state[,(no_t- last_no_t + 1L):no_t,]
  }

  out <- list(metadata = metadata,
              parameters = parameters,
              dim_names = dimnames(xls$latent_state),
              latent_state = xls$latent_state)

  if(is.null(file_name)){
    file_name <- paste0("adapop_pd_", as.Date(out$metadata$written), "_", substr(out$metadata$object_sha,1,6), ".json")
  }
  fp <- file.path(output_dir, file_name)
  jsonlite::write_json(out, path = fp, auto_unbox = TRUE, pretty = TRUE, digits = 3)
}

percentile_col_names <- function(x){
  cn <- paste0("p", as.numeric(gsub("X", "", x))*100)
  gsub(pattern = "\\.", "_", cn)
}

ada_metadata <- function(x){
  checkmate::assert_class(x, "poll_of_polls")
  metadata <- list(created = get_stan_date(x),
                   written = Sys.time(),
                   model = x$model,
                   parties = x$y,
                   object_sha = x$sha,
                   object_git_sha = x$git_sha,
                   time_scale = x$time_scale,
                   no_polls = length(x$polls_data),
                   time_range = time_range(x$polls_data))
  metadata
}
