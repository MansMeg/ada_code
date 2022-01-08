#' Create a [polls_data] object
#'
#' @param y a named [data.frame] with variables of interest to estimate over time.
#' @param house a factor variable indicating polling house (of same length as rows in [y])
#' @param publish_date the date of when the poll was published
#' @param start_date the start date of the data collection period of each poll
#' @param end_date the end date of the data collection period of each poll
#' @param n the (integer) sample size of each poll
#' @param poll_id the id of each poll. Deafult is [row.names] of [y].
#'
#' @param x an object  to convert to a [polls_data] object.
#' @param ... further arguments to methods.
#'
#' @examples
#' data(swedish_polls_curated)
#' pd <- polls_data(y = swedish_polls_curated[,3:11],
#'                  house = swedish_polls_curated$Company,
#'                  publish_date = swedish_polls_curated$PublDate,
#'                  start_date = swedish_polls_curated$collectPeriodFrom,
#'                  end_date = swedish_polls_curated$collectPeriodTo,
#'                  n = swedish_polls_curated$n)
#'
#' @export
polls_data <- function(y, house, publish_date, start_date, end_date, n, poll_id = NULL){
  # Assert all contents
  checkmate::assert_data_frame(y)
  checkmate::assert_named(y)
  checkmate::assert_factor(house, any.missing = FALSE, len = nrow(y))
  checkmate::assert_date(publish_date, any.missing = FALSE, len = nrow(y))
  checkmate::assert_date(start_date, all.missing = FALSE, len = nrow(y))
  checkmate::assert_date(end_date, all.missing = FALSE, len = nrow(y))
  n <- checkmate::assert_integerish(n, all.missing = FALSE, len = nrow(y), lower = 1, coerce = TRUE)
  checkmate::assert_character(poll_id, any.missing = FALSE, len = nrow(y), unique = TRUE, null.ok = TRUE)

  # Construct object
  if(length(poll_id) == 0) poll_id <- row.names(y)
  pd <- list(poll_info = tibble::tibble(.poll_id = poll_id,
                                       .house = house,
                                       .publish_date = publish_date,
                                       .start_date = start_date,
                                       .end_date = end_date,
                                       .n = n),
             y = tibble::as_tibble(cbind(tibble::tibble(.poll_id = poll_id), y)))
  class(pd) <- "polls_data"
  pd$time_range <- time_range(pd)

  # Assert object
  assert_polls_data(x = pd)

  pd
}

assert_polls_data <- function(x, min.rows = NULL, min.cols = NULL){
  checkmate::assert_class(x, classes = "polls_data")
  checkmate::assert_class(x$y, "tbl_df")
  checkmate::assert_named(x$y)
  # We can have different types of data in a poll of polls object. The model restrict the data
#  for(j in 2:ncol(x$y)){
#    checkmate::assert_numeric(x$y[[j]], lower = 0.00001, upper = 0.99999, .var.name = names(x$y)[j])
#  }
  checkmate::assert_named(x$y)
  checkmate::assert_data_frame(x$y, min.rows = min.rows, min.cols = min.cols)
  checkmate::assert_names(names(x$y), must.include = ".poll_id")
  checkmate::assert_class(x$poll_info, "tbl_df")
  checkmate::assert_names(names(x$poll_info), identical.to = c(".poll_id", ".house", ".publish_date", ".start_date", ".end_date", ".n"))

  checkmate::assert_character(x$y$.poll_id, any.missing = FALSE, len = nrow(x$y), unique = TRUE)

  checkmate::assert_character(x$poll_info$.poll_id, any.missing = FALSE, len = nrow(x$y), unique = TRUE)
  checkmate::assert_factor(x$poll_info$.house, any.missing = FALSE, len = nrow(x$y))
  checkmate::assert_date(x$poll_info$.publish_date, any.missing = FALSE, len = nrow(x$y))
  if(nrow(x$y) > 0){
    checkmate::assert_date(x$poll_info$.start_date, all.missing = FALSE, len = nrow(x$y))
    checkmate::assert_date(x$poll_info$.end_date, all.missing = FALSE, len = nrow(x$y))
  }

  publ_end_not_na <- !is.na(x$poll_info$.publish_date) & !is.na(x$poll_info$.end_date)
  end_start_not_na <- !is.na(x$poll_info$.start_date) & !is.na(x$poll_info$.end_date)
  checkmate::assert_true(all(x$poll_info$.publish_date[publ_end_not_na] >= x$poll_info$.end_date[publ_end_not_na]))
  checkmate::assert_true(all(x$poll_info$.end_date[end_start_not_na] >= x$poll_info$.start_date[end_start_not_na]))

  checkmate::assert_true(identical(x$poll_info$.poll_id, x$y$.poll_id))
  if(nrow(x$y) > 0){
    checkmate::assert_integer(x$poll_info$.n, all.missing = FALSE, len = nrow(x$y), lower = 1)
  }
  checkmate::assert_date(x$time_range)
  checkmate::assert_names(names(x$time_range), identical.to = c("from", "to"))
  checkmate::assert_true(x$time_range["from"] < x$time_range["to"])
  return(invisible(TRUE))
}

#' @export
print.polls_data <- function(x, ...){
  polls_data_summary(x)
  print(as.data.frame(x))
}

polls_data_summary <- function(x){
  tr <- time_range(x)
  cat("A polls_data object with ",
      nrow(x$poll_info), " polls from ",
      length(unique(x$poll_info$.house)),
      " houses \nthat range from ", as.character(tr["from"]),
      " to ", as.character(tr["to"]),
      ".\n ", sep = "")
}

known_state_summary <- function(x){
  if(is.null(x)){
    cat("No known states\n")
  } else {
    cat("A known state object with ",
        nrow(x), " known states\nthat range from ",
        as.character(x$date[which.min(x$date)]),
        " to ",
        as.character(x$date[which.max(x$date)]),
        ".\n ", sep = "")
  }
}

#' @export
as.data.frame.polls_data <- function(x, ...){
  dplyr::bind_cols(x$y, x$poll_info[,-1])
}

#' Subset a [polls_data] object.
#'
#' @param x a [polls_data]
#' @param subset,select,drop,... see [subset()]
#'
#' @export
subset.polls_data <- function(x, subset, select, drop = FALSE, ...){
  checkmate::assert_logical(subset)
  checkmate::assert_flag(drop)
  x$poll_info <- subset(x$poll_info, subset, drop = drop, ...)
  x$y <- subset(x$y, subset, select, drop = drop, ...)
  assert_polls_data(x)
  x
}

#' @rdname polls_data
#' @export
as.polls_data <- function(x, ...){
  UseMethod("as.polls_data")
}

#' @export
as.polls_data.list <- function(x, ...){
  class(x) <- "polls_data"
  as.polls_data(x)
}

#' @export
as.polls_data.polls_data <- function(x, ...){
  assert_polls_data(x)
  x
}

#' @rdname polls_data
#' @param keep_poll_id Should the [poll_id]
#' @param i row in [polls_data] to select.
#' @export
`[.polls_data` <- function(x, i, keep_poll_id = TRUE, ...) {
  x$poll_info <- x$poll_info[i,]
  x$y <- x$y[i,]
  if(!keep_poll_id){
    poll_ids(x) <- NULL
  }
  assert_polls_data(x)
  x
}

#' @export
length.polls_data <- function(x){
  nrow(x$poll_info)
}

#' Get and set elements of a [data_polls] object
#'
#' @param x a [data_polls] object.
#' @param value a value to set.
#'
#' @export
poll_id <- function(x){
  checkmate::assert_class(x, "polls_data")
  x$poll_info$.poll_id
}

#' @rdname poll_id
#' @export
poll_ids <- poll_id

#' @rdname poll_id
#' @export
`poll_id<-` <- function(x, value){
  checkmate::assert_class(x, "polls_data")
  if(is.null(value)) value <- as.character(1:length(x))
  checkmate::assert_character(value, any.missing = FALSE, unique = TRUE, len = nrow(x$y))
  x$y$.poll_id <- value
  x$poll_info$.poll_id  <- value
  assert_polls_data(x)
  x
}

#' @rdname poll_id
#' @export
`poll_ids<-` <- `poll_id<-`

#' @rdname poll_id
#' @export
start_date <- function(x){
  checkmate::assert_class(x, "polls_data")
  x$poll_info$.start_date
}
#' @rdname poll_id
#' @export
start_dates <- start_date

#' @rdname poll_id
#' @export
`start_date<-` <- function(x, value){
  checkmate::assert_class(x, "polls_data")
  checkmate::assert_date(value, len = nrow(x$y))
  x$poll_info$.start_date  <- value
  assert_polls_data(x)
  x
}
#' @rdname poll_id
#' @export
`start_dates<-` <- `start_date<-`

#' @rdname poll_id
#' @export
end_date <- function(x){
  checkmate::assert_class(x, "polls_data")
  x$poll_info$.end_date
}
#' @rdname poll_id
#' @export
end_dates <- end_date

#' @rdname poll_id
#' @export
`end_date<-` <- function(x, value){
  checkmate::assert_class(x, "polls_data")
  checkmate::assert_date(value, len = nrow(x$y))
  x$poll_info$.end_date  <- value
  assert_polls_data(x)
  x
}
#' @rdname poll_id
#' @export
`end_dates<-` <- `end_date<-`

#' @rdname poll_id
#' @export
publish_date <- function(x){
  checkmate::assert_class(x, "polls_data")
  x$poll_info$.publish_date
}
#' @rdname poll_id
#' @export
publish_dates <- publish_date

#' @rdname poll_id
#' @export
`publish_date<-` <- function(x, value){
  checkmate::assert_class(x, "polls_data")
  checkmate::assert_date(value, len = nrow(x$y))
  x$poll_info$.publish_date  <- value
  assert_polls_data(x)
  x
}
#' @rdname poll_id
#' @export
`publish_dates<-` <- `publish_date<-`


#' @rdname poll_id
#' @export
n <- function(x){
  checkmate::assert_class(x, "polls_data")
  x$poll_info$.n
}

#' @rdname poll_id
#' @export
collection_midpoint_dates <- function(x){
  checkmate::assert_class(x, "polls_data")
  start_dates(x) + floor((end_dates(x) - start_dates(x)) / 2)
}

#' @rdname poll_id
#' @export
houses <- function(x){
  checkmate::assert_class(x, "polls_data")
  x$poll_info$.house
}

#' @rdname poll_id
#' @export
`n<-` <- function(x, value){
  checkmate::assert_class(x, "polls_data")
  value <- checkmate::assert_integerish(value, any.missing = FALSE, len = nrow(x$y), coerce = TRUE)
  x$poll_info$.n  <- value
  assert_polls_data(x)
  x
}


#' @rdname poll_id
#' @export
y <- function(x){
  checkmate::assert_class(x, "polls_data")
  x$y
}

#' @rdname poll_id
#' @export
`y<-` <- function(x, value){
  checkmate::assert_class(x, "polls_data")
  checkmate::assert_data_frame(value)
  checkmate::assert_names(names(value), must.include = ".poll_id")
  x$y  <- value
  assert_polls_data(x)
  x
}

#' Compute standard errors for poll estimates
#'
#' @param x a [polls_data] object
#' @param y variable to compute standrad error for
#' @param type type of standard error estimator
#' @export
standard_error <- function(x, y, type = "binomial"){
  checkmate::assert_class(x, "polls_data")
  checkmate::assert_choice(y, colnames(y(x)))
  checkmate::assert_choice(type, "binomial")
  p <- y(x)[,y]
  se <- sqrt(p*(1-p)/n(x))
  se
}

