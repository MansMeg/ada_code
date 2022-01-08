month_abbr_en <- function() c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


#' Get and set TEST_POP_MODELS env variable
#'
#' @param x value to set it to
test_pop_models_env <- function() {
  env <- Sys.getenv("TEST_POP_MODELS")
  if(nzchar(env)){
    return(as.logical(as.integer(env)))
  } else {
    return(FALSE)
  }
}

#' @rdname test_pop_models_env
set_test_pop_models_env <- function(x) {
  .Deprecated("test_stan_full_on_local")
  checkmate::assert_flag(x)
  Sys.setenv(TEST_POP_MODELS = as.integer(x))
}

#' Run different test suites
#'
#' @description
#' These functions are used to test Stan models locally. As default,
#' no stan models are tested locally.
#'
#' To test a basic set of Stan models, those that has been decided to be used,
#' run \code{test_stan_basic_on_local(TRUE)}.
#'
#' To run all tests (the whole test suite),
#' run \code{test_stan_full_on_local(TRUE)} OR run the test suite in the
#' \code{test} git branch.
#'
#' To not any longer run a more extensive test suite, run
#' \code{test_stan_basic_on_local(FALSE)} or \code{test_stan_full_on_local(FALSE)}.
#'
#' Note! If in git branch test, all test will always be run.
#'
#' @details
#' Running the test suite without stan models take less than a minute,
#' running the basic models take roughly less than 10 minutes as of now.
#'
#' Note! The testuites checks for the environment variables
#' TEST_STAN_BASIC_ON_LOCAL and TEST_STAN_FULL_ON_LOCAL to run the tests.
#' Hence this can
#'
#' @param x value to set it to
test_stan_basic_on_local <- function() {
  env <- Sys.getenv("TEST_STAN_BASIC_ON_LOCAL")
  if(nzchar(env)){
    return(as.logical(as.integer(env)))
  } else {
    return(FALSE)
  }
}

#' @rdname test_stan_basic_on_local
set_test_stan_basic_on_local <- function(x) {
  checkmate::assert_flag(x)
  Sys.setenv(TEST_STAN_BASIC_ON_LOCAL = as.integer(x))
}

#' @rdname test_stan_basic_on_local
test_stan_full_on_local <- function() {
  if(on_local_test_branch()) return(TRUE)
  env <- Sys.getenv("TEST_STAN_FULL_ON_LOCAL")
  if(nzchar(env)){
    return(as.logical(as.integer(env)))
  } else {
    return(FALSE)
  }
}

#' @rdname test_stan_basic_on_local
set_test_stan_full_on_local <- function(x) {
  checkmate::assert_flag(x)
  Sys.setenv(TEST_STAN_FULL_ON_LOCAL = as.integer(x))
}


#' Check if the tests are run in a specific git branch
#' or on a certain CI.
#'
#' @details
#' If not in git repo, FALSE is returned.
#'
#' @param name The name of a git branch to check
on_local_branch <- function(name){
  gr <- try(git2r::repository(), silent = TRUE)
  if(inherits(gr, "try-error")) return(FALSE)
  identical(name, git2r::repository_head(gr)$name)
}

#' @rdname on_local_branch
on_local_test_branch <- function() on_local_branch("test")

#' @rdname on_local_branch
on_github_actions <- function() identical(Sys.getenv("GITHUB_ACTIONS"), "true")

#' @rdname on_local_branch
on_github_actions_branch <- function(name) {
  identical(Sys.getenv("GITHUB_REF"), name)
}

#' @rdname on_local_branch
on_github_actions_test_branch <- function() {
  on_github_actions_branch("refs/heads/test")
}

#' @rdname on_local_branch
get_working_directory_path <- function() {
  x <- getwd()
  # If on Travis - use Travis build path
  # To handle covr::codecov, that test package in temp folder
  # if (on_travis()) x <- Sys.getenv("TRAVIS_BUILD_DIR")
  # If on Appveyor - use Appveyor build path
  # if (on_appveyor()) x <- Sys.getenv("APPVEYOR_BUILD_FOLDER")
  # If on github actions, use github actions build path
  gx <- try(git2r::repository(), silent = TRUE)
  if(!inherits(gx, "try-error")) x <- gsub(x = gx$path, pattern = "\\.git", replacement = "")
  if (on_github_actions()) x <- Sys.getenv("GITHUB_WORKSPACE")
  x
}


#' Return a data.frame with dates from a list of pop objects
#'
#' @param x a list of pop objects
#'
model_dates_data_frame <- function(x){
  checkmate::assert_list(x)
  for(i in seq_along(x)){
    checkmate::assert_class(x[[i]], "poll_of_polls")
    checkmate::assert_subset(x[[i]]$y, x[[1]]$y)
  }
  trk <-  function(x){
    ds <- x$known_state$date
    if(is.null(ds)){
      c(from = as.Date(NA), to = as.Date(NA))
    } else {
      c(from = ds[which.min(ds)], to = ds[which.max(ds)])
    }
  }

  dat <- data.frame(model_no = 1:length(x))
  dat$model_start_date <- unname(do.call("c", lapply(x, function(x) time_range(x$time_line)["from"])))
  dat$model_end_date <- unname(do.call("c", lapply(x, function(x) time_range(x$time_line)["to"])))
  dat$polls_start_date <- unname(do.call("c", lapply(x, function(x) time_range(x$polls_data)["from"])))
  dat$polls_end_date <- unname(do.call("c", lapply(x, function(x) time_range(x$polls_data)["to"])))
  dat$known_state_start_date <- unname(do.call("c", lapply(x, function(x) trk(x)["from"])))
  dat$known_state_end_date <- unname(do.call("c", lapply(x, function(x) trk(x)["to"])))
  dat
}


remove_file_extension <- function(x){
  checkmate::assert_string(x)
  splt <- strsplit(x, "\\.")[[1]]
  substr(x, 1, nchar(x) - nchar(splt[length(splt)]) - 1)
}


#' Extract parameters from the stan object
#'
#' @param object an object to extract parameter draws from
#' @param ... arguments supplied to rstan::extract.
#' @export
extract <- function(object, ...){
  UseMethod("extract")
}

#' @rdname extract
#' @export
extract.poll_of_polls <- function(object, ...){
  rstan::extract(object$stan_fit, ...)
}

#' Extract the data when Stan was run
#'
#' @param object a [poll_of_polls] object
#'
#' @export
get_stan_date <- function(object){
  checkmate::assert_class(object, "poll_of_polls")
  lubridate::parse_date_time(substr(object$stan_fit@date,5,nchar(object$stan_fit@date)), orders = "%b %d %H:%M:%S %Y", tz = Sys.timezone())
}

#' Extract the data when Stan was run
#'
#' @param x a numeric vector of loged values
#'
#' @export
logMeanExp <- function(x) {
  logS <- log(length(x))
  matrixStats::logSumExp(x) - logS
}

