#' Curate the Swedish polls dataset
#'
#' @param x a SwedishPolls data.frame
#' @param parties parties to curate
#'
#' @export
curate_swedish_polls <- function(x, parties = c("M", "L", "C", "KD", "S", "V", "MP", "SD", "FI")){
  checkmate::assert_data_frame(x)
  checkmate::assert_character(parties)
  checkmate::assert_names(names(x), must.include = parties)

  # Remove polls without publication date
  to_rm <- is.na(x$PublDate)
  x <- x[!to_rm,]
  if(any(!to_rm)) message("Polls without publication date has been removed.")

  # Remove polls without known house
  to_rm <- is.na(x$PublDate)
  x <- x[!to_rm,]
  if(any(!to_rm)) message("Polls without known house has been removed.")

  # Correct polls with approximate time periods to not overlap publication date
  idx <- x$PublDate >= x$collectPeriodTo
  idx[is.na(idx)] <- TRUE
  x$collectPeriodTo[!idx] <- NA
  idx <- x$collectPeriodTo >= x$collectPeriodFrom
  idx[is.na(idx)] <- TRUE
  x$collectPeriodFrom[!idx] <- NA

  for(p in parties){
    x[, p] <- x[, p]/100
  }
  x
}
