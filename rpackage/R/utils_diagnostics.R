#' Extract diagnostics as data frames
#'
#' @param x a [poll_of_polls] object
#'
#' @export
extract_diagnostics_as_df <- function(x){
  checkmate::assert_class(x, "poll_of_polls")
  res <- data.frame(model = x$model, parties = paste(x$y, collapse = ", "), time_scale = x$time_scale)
  res <- cbind(res, as.data.frame(get_model_model_arguments(x, all = TRUE)))
  res <- cbind(res, as.data.frame(get_model_diagnostics(x)))
  res$sha <- x$sha
  res
}


