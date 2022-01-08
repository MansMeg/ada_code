#' Plot a x vs kappa plot
#'
#' @param pop a [poll_of_polls] object with industry bias using kappa.
#' @param y a variable name in the [poll_of_polls] object.
#' @param t time line time point to plot
#' @export
plot_kappa_vs_x <- function(pop, y, t){
  checkmate::assert_class(pop, "pop_model8a")
  ls <- latent_state(pop)
  checkmate::assert_int(t, lower = 1, upper = pop$stan_data$stan_data$T)
  y_names <- dimnames(ls$latent_state)[[3]]
  checkmate::assert_choice(y, choices = y_names)
  kappa <- extract(pop, pars = "kappa")

  y_idx <- which(y == y_names)
  ks <- pop$known_state[dates_in_time_line(pop$known_state$date, pop$time_line),]
  k_idx <- get_dates_next_known_state_index(dates = pop$time_line$time_line[, "date", drop=TRUE], ks)
  plt <- ggplot2::qplot(kappa$kappa[,k_idx[t],y_idx], ls$latent_state[,t, y_idx], ylab = paste0("x[",t,"]"), xlab = paste0("kappa[",k_idx[t],"]"), main = y)
  plt
}
