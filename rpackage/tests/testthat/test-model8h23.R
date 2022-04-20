context("model8h")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)
# options(mc.cores = parallel::detectCores())
if(FALSE){ # For debugging
  library(testthat)
  library(adapop)
}


test_that("Test model 8h2 and 8h3", {
  # First we test that we get a similar result with 8g and 8g1
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")


  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 150,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)

  expect_message(
    suppressWarnings(
      sd <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8h3",
                            known_state = known_state,
                            hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                    use_multivariate_version = 0))
    )
  )

  # Check handling of NA in y data used as the first poll
  spd2 <- spd
  spd2$y[, "x3"] <- NA
  expect_message(
    suppressWarnings(
      sd <- stan_polls_data(x = spd2,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8h3",
                            known_state = known_state,
                            hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                    use_multivariate_version = 0))
    )
  )
  expect_gt(sd$stan_data$x1_prior_p[1], 0)
  expect_gt(sd$stan_data$x1_prior_p[2], 0)
  expect_gt(sd$stan_data$x1_prior_p[3], 0)

  spd2$y[, "x4"] <- NA
  expect_message(
    suppressWarnings(
      sd <- stan_polls_data(x = spd2,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8h3",
                            known_state = known_state,
                            hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                    use_multivariate_version = 0))
    )
  )
  expect_gt(sd$stan_data$x1_prior_p[1], 0)
  expect_gt(sd$stan_data$x1_prior_p[2], 0)
  expect_gt(sd$stan_data$x1_prior_p[3], 0)



  # Check that we get identical lpd
  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 0L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 0L,
               use_softmax = 1L)

  expect_silent(pop8h3_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8h3 <- poll_of_polls(y = parties,
                                                model = "model8h3",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = cfg,
                                                warmup = 0,
                                                iter = 3,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  expect_silent(pop8h2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8h2 <- poll_of_polls(y = parties,
                                                model = "model8h2",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = cfg,
                                                warmup = 0,
                                                iter = 3,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  pn8h3 <- parameter_names(pop8h3)
  pn8h2 <- parameter_names(pop8h2)
  expect_equal(pn8h3, pn8h2)

  no_up3 <- get_num_upars(pop8h3)
  no_up2 <- get_num_upars(pop8h2)
  expect_equal(no_up3, no_up2)

  lsh3 <- latent_state(pop8h3)
  lsh2 <- latent_state(pop8h2)

})


test_that("Test to run model 8h3", {
  # First we test that we get a similar result with 8g and 8g1
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions())
  skip("Takes too long time to do. Mainly used for manual inspection.")

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")


  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 150,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)

  expect_message(
    suppressWarnings(
      sd <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8h3",
                            known_state = known_state,
                            hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                    use_multivariate_version = 0))
    )
  )

  # Check that we get identical lpd
  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 0L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 0L,
               use_softmax = 1L)

  expect_silent(pop8h3_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8h3 <- poll_of_polls(y = parties,
                                                model = "model8h3",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = cfg,
                                                warmup = 1000,
                                                iter = 2000,
                                                chains = 4,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  expect_silent(pop8h2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8h2 <- poll_of_polls(y = parties,
                                                model = "model8h2",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = cfg,
                                                warmup = 0,
                                                iter = 3,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  party <- "x3"
  plt <- plot(pop8h3, party) + geom_known_state(pop8h3, party) + geom_pop_line(pop8h3, txdf[[party]]) + ggplot2::theme_bw()
  party <- "x4"
  plt <- plot(pop8h3, party) + geom_known_state(pop8h3, party) + geom_pop_line(pop8h3, txdf[[party]]) + ggplot2::theme_bw()

})



