context("model8g2")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)
# options(mc.cores = parallel::detectCores())
if(FALSE){ # For debugging
  library(testthat)
  library(adapop)
}



test_that("Test model 8g2", {
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
                            model = "model8g2",
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
               use_softmax = 0L)

  expect_silent(pop8g1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8g1 <- poll_of_polls(y = parties,
                                               model = "model8g1",
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

  expect_silent(pop8g2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8g2 <- poll_of_polls(y = parties,
                                                model = "model8g2",
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


  pn8g1 <- parameter_names(pop8g1)
  pn8g2 <- parameter_names(pop8g2)
  expect_equal(pn8g1, pn8g2)

  no_up1 <- get_num_upars(pop8g1)
  no_up2 <- get_num_upars(pop8g2)
  expect_equal(no_up1, no_up2)

  par_values <- rep(0, no_up1)
  lp1a <- log_prob(pop8g1, par_values)
  lp1b  <- log_prob(pop8g2, par_values)
  expect_equal(lp1a, lp1b)

  set.seed(4712)
  par_values <- rnorm(no_up1)
  lp2a <- log_prob(pop8g1, par_values)
  lp2b  <- log_prob(pop8g2, par_values)
  expect_equal(lp2a, lp2b)

  ls1 <- latent_state(pop8g1)
  ls2 <- latent_state(pop8g2)



  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 0L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 2L,
               use_softmax = 1L)

  expect_silent(pop8g1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8g1 <- poll_of_polls(y = parties,
                                                model = "model8g1",
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

  expect_silent(pop8g2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8g2 <- poll_of_polls(y = parties,
                                                model = "model8g2",
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


  pn8g1 <- parameter_names(pop8g1)
  pn8g2 <- parameter_names(pop8g2)
  expect_equal(pn8g1, pn8g2)

  no_up1 <- get_num_upars(pop8g1)
  no_up2 <- get_num_upars(pop8g2)
  expect_equal(no_up1, no_up2)

  par_values <- rep(0, no_up1)
  lp1a <- log_prob(pop8g1, par_values)
  lp1b  <- log_prob(pop8g2, par_values)
  expect_equal(lp1a, lp1b)

  set.seed(4712)
  par_values <- rnorm(no_up1)
  lp2a <- log_prob(pop8g1, par_values)
  lp2b  <- log_prob(pop8g2, par_values)
  expect_equal(lp2a, lp2b)

  ls1 <- latent_state(pop8g1)
  ls2 <- latent_state(pop8g2)

})
