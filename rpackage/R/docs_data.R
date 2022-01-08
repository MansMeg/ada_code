#' Four true latent states used in experiments
#'
#' @details
#' See data-raw folder for exact construction.
#'
#' @format A list of length four with numeric vectors of length 100.
#'
"x_test"

#' A subset of swe_polls stored as a [polls_data] object
#'
#' @details
#' All polls from [swe_polls] without missing data
#' during 2010-01-01 to 2019-12-31.
#' See data-raw folder for exact construction.
#'
#' @format a [polls_data] object with 1007 polls.
#'
"pd_test"

#' Map between houses and house names
#'
#' @format A data.frame with house, house name and country.:
#'
"house_map"

#' Swedish polls data
#'
#' @format A data frame with 1565 polls and 9 parties:
#'
#' @source \url{http://www.github.com/MansMeg/SwedishPolls}
"swedish_polls"

#' @rdname spanish_polls
"swedish_polls_curated"

#' German polls data
#'
#' @format A data frame with 4609 polls and 6 parties:
#'
#' @source \url{http://www.wahlrecht.de/umfragen/}
"german_polls"

#' @rdname german_polls
"german_polls_curated"

#' Spanish polls data
#'
#' @format A data frame with 1852 polls and 7 parties:
#'
#' @source \url{http://www.wikipedia.com}
"spanish_polls"

#' @rdname spanish_polls
"spanish_polls_curated"

#' Swedish election data
#'
#' @format A data frame with 4 elections, 2006-2018
#'
#' @source \url{http://www.wikipedia.com}
"swedish_elections"

#' German election data
#'
#' @format A data frame with 4 elections, 2005-2017
#'
#' @source \url{http://www.wikipedia.com}
"german_elections"

#' Spanish election data
#'
#' @format A data frame with 6 elections, 2008-2019
#'
#' @source \url{http://www.wikipedia.com}
"spanish_elections"


#' Party time ranges
#'
#' @details
#' Time ranges for political parties
#'
#' @format A named list (parties) with starting and ending dates.
#'
"swedish_party_time_ranges"

#' @rdname swedish_party_time_ranges
"german_party_time_ranges"

#' @rdname swedish_party_time_ranges
"spanish_party_time_ranges"

