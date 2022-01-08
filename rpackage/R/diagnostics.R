#' Extract the percentiles of the divergent transistions
#'
#' @param x a [poll_of_polls] object
#'
#' @return a matrix of the percentiles of the divergent transitions
#'
#' @export
divergent_transitions_percentiles <- function(x){
  checkmate::assert_class(x, "poll_of_polls")
  div_idx <- which(rstan::get_divergent_iterations(x$stan_fit))
  #  mtd_idx <- which(get_max_treedepth_iterations(x$stan_fit))
  draws_mat <- as.matrix(x$stan_fit)
  draws_mat_div <- draws_mat[div_idx,]
  draws_mat_nodiv <- draws_mat[-div_idx,]
  percentiles <- draws_mat_div

  for(i in 1:ncol(draws_mat_nodiv)){
    empf <- sort(draws_mat_nodiv[,i])
    for(j in 1:nrow(draws_mat_div)){
      percentiles[j, i] <- mean(empf < draws_mat_div[j,i])
    }
  }
  rownames(percentiles) <- as.character(div_idx)
  percentiles
}

#' Extract more detailed diagnostic information
#'
#' @param x a [poll_of_polls] object
#' @param m a limit to extract Rhat values above
#' @export
Rhat_is_above_m_by_parameter_block <- function(x, m = 1.1){
  nms <- names(Rhat_is_above_m(x, m = m))
  table(parameters_names_remove_indecies(nms))
}

#' @rdname Rhat_is_above_m_by_parameter_block
#' @export
Rhat_is_above_m <- function(x, m = 1.1){
  bool <- x$diagnostics$Rhat > m
  bool[is.na(bool)] <- FALSE
  rh <- x$diagnostics$Rhat[bool]
  rh
}

#' @rdname Rhat_is_above_m_by_parameter_block
#' @export
Rhat_is_NA_by_parameter_block <- function(x){
  rhs <- Rhat_is_NA(x)
  table(parameters_names_remove_indecies(names(rhs)))
}

#' @rdname Rhat_is_above_m_by_parameter_block
#' @export
Rhat_is_NA <- function(x){
  bool <- is.na(x$diagnostics$Rhat)
  rh <- x$diagnostics$Rhat[bool]
  rh
}



#' Extract minimal eigen value per iteration
#'
#' @param x a [poll_of_polls] object
#' @export
diagnose_extract_min_eigen_value_Omega <- function(x){
  checkmate::assert_choice(x$model, choices = c("model8g", "model8g1", "model8g2", "model8g3"))

  omega_array <- extract(x$stan_fit, "Omega")[[1]]
  res <- matrix(-100, nrow = dim(omega_array)[1], ncol = dim(omega_array)[2])
  dimnames(res) <- dimnames(omega_array)[1:2]
  for(i in 1:nrow(res)){
    for(j in 1:ncol(res)){
      res[i,j] <- min(eigen(omega_array[i,j,,])$values)
    }
  }
  res
}


#' Get adaptation info
#'
#' @description
#' Extract the adaptation info (step size and diagonal invers mass matrix)
#' from a [poll_of_polls] object
#'
#' @param x a [poll_of_polls]
#' @param as.list return an R list. Defaults to [FALSE].
#' @param ... further arguemt past to [rstan::get_adaptation_info]
#'
#' @export
get_adaptation_info <- function(x, as.list = FALSE, ...){
  ai <- rstan::get_adaptation_info(x$stan_fit, ...)
  ai <- lapply(ai, parse_adaption_information)
}

parse_adaption_information <- function(x){
  checkmate::assert_string(x)
  x <- as.list(strsplit(x, "\n")[[1]])
  res <- list(adaption_terminated = FALSE,
              step_size = 0.0,
              diag_inv_mass_matrix = c(0.0, 0.0))
  res$adaption_terminated <- grepl(x[[1]], pattern = "Adaptation terminated")
  res$step_size <- as.numeric(sub(x[[2]], pattern = "[^0-9]+", replacement = ""))
  res$diag_inv_mass_matrix <- as.numeric(strsplit(sub(x[[4]], pattern = "#", replacement = ""), split = ", ")[[1]])
  res
}


#' Get parameter names from unconstrained space
#'
#' @description
#' The function returns the name and indecies of all parameters on the constrained
#' space that are affected by the unconstrained parameter.
#'
#' @param stanfit a [stanfit] object to extract parameter names from
#' @param up_idx the index of parameters at the constrained space to extract names for
#'
#' @return a [data.frame] with one row per parameter affected by the unconstrained
#' @export
parameter_names_from_unconstrained_index <- function(stanfit, up_idx){
  checkmate::assert_class(stanfit, "stanfit")
  if(is.logical(up_idx)) up_idx <- which(up_idx)
  checkmate::assert_integerish(up_idx, lower = 1L, upper = rstan::get_num_upars(stanfit))

  no_up <- rstan::get_num_upars(stanfit)
  u_pars_zero <- rep(0, no_up)
  no_change <- rstan::constrain_pars(stanfit, u_pars_zero)

  full_res <- list()
  for(j in seq_along(up_idx)){
    u_pars <- rep(0, no_up)
    u_pars[up_idx[j]] <- 1
    par_change <- rstan::constrain_pars(stanfit, u_pars)

    res <- list()
    for(i in seq_along(no_change)){
      if(is.null(dim(par_change[[i]]))){
        # Handle scalars (treat as 1d arrays)
        dim(par_change[[i]]) <- 1L
        dim(no_change[[i]]) <- 1L
      }
      array_idx <- which(no_change[[i]] != par_change[[i]], arr.ind = TRUE)

      # return a data.frame with up_index, par_name, par_index
      if(nrow(array_idx) == 0L) {
        res[[i]] <- NULL
      } else {
        par_idx <- apply(array_idx, 1, function(x) paste(as.character(x), collapse = ","))
        res[[i]] <- data.frame(upar_idx = up_idx[j], par_name = names(no_change)[i], par_idx = par_idx)
      }
    }
    full_res[[j]] <- do.call(res, what = rbind)
  }
  res <- do.call(full_res, what = rbind)
  res
}

#' @rdname parameter_names_from_unconstrained_index
#' @export
parameter_block_names_from_unconstrained_index <- function(stanfit, up_idx){
  pn <- parameter_names_from_unconstrained_index(stanfit, up_idx)
  df <- as.data.frame(table(pn$upar_idx, pn$par_name))
  colnames(df) <- c("upar_idx", "par_name", "no_params")
  df
}

#' @rdname parameter_names_from_unconstrained_index
#' @export
parameter_unique_names_from_unconstrained_index <- function(stanfit, up_idx){
  pn <- parameter_names_from_unconstrained_index(stanfit, up_idx)
  pbn <- parameter_block_names_from_unconstrained_index(stanfit, up_idx)
  pn <- merge(pn, pbn, by = c("upar_idx", "par_name"))
  pn <- pn[pn$no_params == 1L,1:3]
  df <- as.data.frame(table(pn$par_idx, pn$par_name))
  colnames(df) <- c("par_idx", "par_name", "no_params")
  pn$row_names <- rownames(pn)
  pn <- merge(pn, df, by = c("par_idx", "par_name"))
  pn <- pn[pn$no_params == 1L,]
  pn <- pn[order(pn$upar_idx, as.numeric(pn$row_names)),1:3]
  rownames(pn) <- NULL
  pn <- pn[, c("upar_idx", "par_name", "par_idx" )]
  pn
}
