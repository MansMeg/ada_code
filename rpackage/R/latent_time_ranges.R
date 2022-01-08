#' Construct a list of [time_range]s
#'
#' @param x a list with a character vector with either "from" or "to"
#' @param y all categories as a chacrater vector
#' @param mtr a model [time_range] that will be used as default
#' @param check_dates assert that the dates of [x] is in the [mtr] interval
#' @export
setup_latent_time_ranges <- function(x, y, mtr, check_dates = TRUE){
  assert_latent_time_range_list(x)
  ltr <- list()

  additional_names <- !names(x) %in% y
  if(any(additional_names)){
    warning("'",
            paste0(names(x)[additional_names], collapse = "', '"),
            "' in latent_time_range are ignored.", call. = FALSE)
  }

  for(i in seq_along(y)){
    ltr[[y[i]]] <- mtr
    if(is.null(x[[y[i]]])) next
    if(!is.null(x[[y[i]]][["from"]])) ltr[[y[i]]][["from"]] <- as.Date(x[[y[i]]][["from"]])
    if(!is.null(x[[y[i]]][["to"]])) ltr[[y[i]]][["to"]] <- as.Date(x[[y[i]]][["to"]])
  }
  if(check_dates) assert_latent_time_ranges(x = ltr, y = y, mtr)
  ltr
}

assert_latent_time_range_list <- function(x){
  if(is.null(x)) return(invisible(TRUE))
  checkmate::assert_list(x)
  for(i in seq_along(x)){
    checkmate::assert_list(x[[i]])
    checkmate::assert_names(names(x[[i]]), subset.of = c("from", "to"))
  }
}

assert_latent_time_ranges <- function(x, y = NULL, mtr = NULL){
  if(is.null(x)) return(invisible(TRUE))
  checkmate::assert_list(x)
  if(!is.null(y)) {
    checkmate::assert_names(names(x), must.include = y)
  }
  for(i in seq_along(x)){
    if(!is.null(mtr)) {
      if(time_ranges_are_disjoint(mtr, x[[i]])) stop("The latent time range of '", names(x)[i], "' (",paste0(x[[i]], collapse = "--"),") is not a part of the model time range (",paste0(mtr, collapse = "--"),").", call. = FALSE)
    }
    assert_time_range(x[[i]])
  }
}


