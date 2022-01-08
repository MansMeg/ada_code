#' Create a latent state object
#'
#' @param x an object to convert to a [latent_state]
#' @param time_line a [time_line] object
#' @param ... further arguments to methods
#'
#' @export
latent_state <- function(x, time_line = NULL, ...){
  UseMethod("latent_state")
}

#' @rdname latent_state
#' @export
latent_state.poll_of_polls <- function(x, time_line = NULL, ...){
  dn <- list(iterations = NULL,
             t = NULL,
             categories = x$y)
  latent_state(x = x$stan_fit, time_line = x$time_line, dimnames = dn)
}

#' @rdname latent_state
#' @param dimnames The dimnames attribute for the latent state array
#' @export
latent_state.stanfit <- function(x, time_line, dimnames, ...){
  checkmate::assert_class(x, "stanfit")
  checkmate::assert_class(time_line, "time_line")
  checkmate::assert_list(dimnames)
  checkmate::assert_names(names(dimnames), identical.to = c("iterations", "t", "categories"))
  checkmate::assert_character(dimnames$categories)

  if(x@model_name %in% c("model2", "model3", "model4", "model5")){
    xs <- rstan::extract(x, pars = "x")[[1]]
    xs <- array(xs, dim = c(nrow(xs),ncol(xs),1))
  } else if (x@model_name %in% c("model6", "model6b", "model6c",
                                "model7",
                                "model8a", "model8a1", "model8a3", "model8a4",
                                "model8b", "model8b1",
                                "model8c", "model8c2",
                                "model8d", "model8d2", "model8d3",
                                "model8e", "model8e2",
                                "model8f",
                                "model9","model10e","model10d","model11a","model11b")){
    xs <- rstan::extract(x, pars = "x")[[1]]
  } else if (x@model_name %in% c("model8f1", "model8f2", "model8f3", "model8g", "model8g1", "model8g2", "model8g3", "model8h2", "model8h3", "model8h4")){
    xs <- rstan::extract(x, pars = "x_pred")[[1]]
  } else if (grepl(x@model_name, pattern = "^model8i[0-9]+$")){
    xs <- rstan::extract(x, pars = "x_pred")[[1]]
  } else {
    stop("'", x@model_name, "' not implemented in latent_state().")
  }
  ls <- list(latent_state = xs, time_line = time_line)
  class(ls) <- c("latent_state", "list")

  if(is.null(dimnames$t)) dimnames$t <- as.character(1:ncol(ls$latent_state))
  if(length(dimnames$categories) + 1L == dim(xs)[3]) dimnames$categories <- c(dimnames$categories, "other")
  attr(ls$latent_state, which = "dimnames") <- dimnames

  assert_latent_state(ls)
  ls
}

assert_latent_state <- function(x){
  checkmate::assert_class(x, "latent_state")
  checkmate::assert_names(names(x), identical.to = c("latent_state", "time_line"))
  checkmate::assert_array(x$latent_state, d = 3)

  checkmate::assert_names(names(dimnames(x$latent_state)), identical.to = c("iterations", "t", "categories"))
  checkmate::assert_integerish(as.integer(dimnames(x$latent_state)[[2]]), min.len = 1)
  checkmate::assert_character(dimnames(x$latent_state)[[3]], any.missing = FALSE)
}


#' @export
`[.latent_state` <- function(x, i, j, k) {
  x$latent_state <- x$latent_state[i,j,k, drop = FALSE]
  assert_latent_state(x)
  x
}

#' Get the latent state for specific dates
#'
#' @param x a [poll_of_polls] or [latent_state] object
#' @param date a [date] vector
#'
#' @export
get_latent_state_for_dates <- function(x, date){
  checkmate::assert_date(date)
  if(inherits(x, "poll_of_polls")) x <- latent_state(x)
  checkmate::assert_class(x, "latent_state")

  tlt <- x$time_line$daily$time_line_t[x$time_line$daily$date == date]
  if(length(tlt) == 0) stop("'date' not in latent state time line.", call. = FALSE)
  x <- x[,tlt,]
  x
}


#' Compute functions of the latent state
#'
#' @param x a [latent_state]
#' @param percentiles percentiles to compute
#' @param dates a vector of date values
#' @param draw_idx a draw index from the posterior
#'
#' @export
latent_state_percentiles <- function(x, percentiles){
  if(!inherits(x, "latent_state")){
    x <- latent_state(x)
  }
  checkmate::assert_class(x, "latent_state")
  checkmate::assert_numeric(percentiles, lower = 0, upper = 1)
  dfl <- list()
  for(k in 1:dim(x$latent_state)[3]){
    df <- as.data.frame(t(apply(x$latent_state[,,k], 2, stats::quantile, percentiles)))
    colnames(df) <- make.names(percentiles)
    df$categories <- dimnames(x$latent_state)$categories[k]
    df$t <- as.integer(colnames(x$latent_state[,,k]))
    dfl[[k]] <- dplyr::left_join(df, x$time_line$time_line, by = "t")
  }
  df <- do.call(rbind, dfl)
  assert_latent_state_stats(df)
  df
}

#' @rdname latent_state_percentiles
#' @export
latent_state_mean <- function(x){
  latent_state_FUN(x, mean, "mean")
}

#' @rdname latent_state_percentiles
#' @export
latent_state_draw <- function(x, draw_idx){
  checkmate::assert_class(x, "poll_of_polls")
  draw_FUN <- function(x) x[draw_idx]
  latent_state_FUN(x, draw_FUN, "draw")
}



#' @rdname latent_state_percentiles
#' @export
latent_state_sd <- function(x){
    latent_state_FUN(x, stats::sd, "sd")
}

#' Compute an arbitrary function of latent states
#'
#' @param x a latent state
#' @param FUN an arbitrary function
#' @param var_name the variable name to use
#'
#'
latent_state_FUN <- function(x, FUN, var_name){
  UseMethod("latent_state_FUN")
}

latent_state_FUN.poll_of_polls <- function(x, FUN, var_name){
  x <- latent_state(x)
  latent_state_FUN(x, FUN, var_name)
}

latent_state_FUN.latent_state <- function(x, FUN, var_name){
  checkmate::assert_class(x, "latent_state")
  checkmate::assert_function(FUN)
  checkmate::assert_string(var_name)

  dfl <- list()
  for(k in 1:dim(x$latent_state)[3]){
    df <- data.frame(apply(x$latent_state[,,k], 2, FUN))
    colnames(df) <- var_name
    df$categories <- dimnames(x$latent_state)$categories[k]
    df$t <- as.integer(colnames(x$latent_state[,,k]))
    dfl[[k]] <- dplyr::left_join(df, x$time_line$time_line, by = "t")
  }
  df <- do.call(rbind, dfl)
  assert_latent_state_stats(df)
  df
}

#' @rdname latent_state_percentiles
#' @export
latent_state_mean_dates <- function(x, dates){
  if(!inherits(x, "latent_state")){
    x <- latent_state(x)
  }
  checkmate::assert_class(x, "latent_state")
  checkmate::assert_date(dates)

  dts <- tibble::tibble(date = dates)
  dts <- dplyr::left_join(dts, x$time_line$daily[, c("time_line_t", "date")], by = "date")
  lsm <- latent_state_mean(x)
  lsm$date <- NULL
  dts <- dplyr::left_join(dts, lsm, by = c("time_line_t" = "t"))
  dts
}


assert_latent_state_stats <- function(x){
  checkmate::assert_data_frame(x)
  checkmate::assert_names(names(x), must.include = c("categories", "t", "date"))
}


#' Subset latent_state objects
#'
#' @param x a [latent_state] object to subet
#' @param iterations subset iterations
#' @param t subset time points (t)
#' @param categories subset categories
#'
#' @return a [latent_state] object
#'
#' @export
latent_state_subset <- function(x, iterations = NULL, t = NULL, categories = NULL){
  checkmate::assert_class(x, "latent_state")

  dn <- dimnames(x$latent_state)
  no_iterations <- dim(x$latent_state)[which(names(dn) == "iterations")]
  all_t_labels <- dimnames(x$latent_state)[[which(names(dn) == "t")]]
  all_categories_labels <- dimnames(x$latent_state)[[which(names(dn) == "categories")]]
  if(is.null(iterations)) iterations <- 1:dim(x$latent_state)[which(names(dn) == "iterations")]
  checkmate::assert_integerish(iterations, lower = 1, upper = no_iterations)
  if(is.null(t)) t <- all_t_labels else checkmate::assert_true(all(t %in% all_t_labels))
  if(is.null(categories)) categories <- all_categories_labels else checkmate::assert_true(all(categories %in% all_categories_labels))

  x$latent_state <- x$latent_state[iterations,t,categories]
  assert_latent_state(x)
  x
}


#' Apply latent_state objects
#'
#' @param x a [latent_state] object to subet
#' @param MARGIN margins to apply over
#' @param FUN function to apply
#' @param ... further arguments to base::apply
#'
#' @return a [latent_state] object
#'
#' @export
latent_state_apply <- function(x, MARGIN, FUN, ...){
  checkmate::assert_class(x, "latent_state")
  checkmate::assert_integerish(MARGIN, lower = 1, upper = 3)
  checkmate::assert_true(2L %in% MARGIN) # Must include the time dimension
  checkmate::assert_function(FUN)

  mc <- match.call()
  dims <- dim(x$latent_state)
  margin_dims <- sort(MARGIN)
  non_margin_dims <- (1:3)[!(1:3 %in% margin_dims)]
  new_dims <- dims
  new_dims[non_margin_dims] <- 1L

  new_dimnames <- dimnames(x$latent_state)
  new_dimnames[non_margin_dims] <-
    lapply(new_dimnames[non_margin_dims], function(X) X[[1]] <- as.character(mc$FUN))

  new_ls <- apply(x$latent_state, margin_dims, FUN, ...)
  dim(new_ls) <- new_dims
  dimnames(new_ls) <- new_dimnames
  x$latent_state <- new_ls
  assert_latent_state(x)
  x
}

#' Bind latent_state objects
#'
#' @param x a [latent_state] object
#' @param y a [latent_state] object
#' @param along the dimension to bind the latent states. See [abind::abind] for details.
#' @param ... further arguments to [abind::abind]
#'
#' @return a [latent_state] object
#'
#' @export
latent_state_abind<- function(x, y, along, ...){
  checkmate::assert_class(x, "latent_state")
  checkmate::assert_class(y, "latent_state")

  new_ls <- abind::abind(x$latent_state, y$latent_state, along = along, ...)
  dn <- dimnames(new_ls)
  names(dn) <- names(dimnames(x$latent_state))
  dimnames(new_ls) <- dn
  x$latent_state <- new_ls
  assert_latent_state(x)
  x
}
