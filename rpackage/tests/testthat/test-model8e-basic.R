context("model8e")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)


test_that("Test model 8e and 8d are identical", {
  # First we test that we get a similar result with 8d as with 8a4
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
                            model = "model8e",
                            known_state = known_state,
                            hyper_parameters = list(sigma_kappa_hyper = 0.001))
    )
  )

  # Simulate new with kappa = 0.02 per year
  kappa_x3 <- 0.05
  kappa_x4 <- -0.05
  spd$y[,"x3"] <-  spd$y[,"x3"] + sd$stan_data$g * kappa_x3
  spd$y[,"x4"] <-  spd$y[,"x4"] + sd$stan_data$g * kappa_x4

  # Check that we get identical lpd
  cfg <-  list()

  expect_silent(pop8e_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8e <- poll_of_polls(y = parties,
                                                model = "model8e",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = cfg,
                                                warmup = 0,
                                                iter = 5,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )
  expect_silent(pop8d_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d3 <- poll_of_polls(y = parties,
                                                model = "model8d3",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = cfg,
                                                warmup = 0,
                                                iter = 5,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  pn8d3 <- parameter_names(pop8d3)
  pn8e <- parameter_names(pop8e)
  # Currently these are included even though they are known
  expect_equal(pn8e[!(pn8e %in% c("alpha_kappa[1]", "alpha_beta_mu[1]", "alpha_beta_sigma[1]"))],
                    pn8d3)

  no_up <- get_num_upars(pop8e)
  expect_equal(no_up, get_num_upars(pop8d3))

  par_values <- rep(0, no_up)
  lp1_8d3 <- log_prob(pop8d3$stan_fit, par_values)
  lp1_8e  <- log_prob(pop8e, par_values)
  expect_equal(lp1_8d3, lp1_8e)

  par_values <- rnorm(no_up)
  lp2_8d3 <- log_prob(pop8d3, par_values)
  lp2_8e  <- log_prob(pop8e, par_values)
  expect_equal(lp2_8d3, lp2_8e)


  # Check that we get identical lpd
  cfg <-  list(sigma_kappa_hyper = 0.01,
               use_industry_bias = 1L,
               use_house_bias = 1L,
               use_design_effects = 1L,
               g_scale = 4,
               use_constrained_house_house_bias = 1L,
               use_constrained_party_house_bias = 1L,
               use_constrained_party_kappa = 1L)

  expect_silent(pop8d_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8e <- poll_of_polls(y = parties,
                                               model = "model8e",
                                               polls_data = spd,
                                               time_scale = time_scale,
                                               known_state = known_state,
                                               hyper_parameters = cfg,
                                               warmup = 0,
                                               iter = 1,
                                               chains = 1,
                                               cache_dir = NULL)
                      )
                    )
                  )
  )
  expect_silent(pop8d_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d3 <- poll_of_polls(y = parties,
                                                model = "model8d3",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = cfg,
                                                warmup = 0,
                                                iter = 1,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  pn8d3 <- parameter_names(pop8d3)
  pn8e <- parameter_names(pop8e)
  # Currently these are included even though they are known
  expect_equal(pn8e[!(pn8e %in% c("alpha_kappa[1]", "alpha_beta_mu[1]", "alpha_beta_sigma[1]"))],
               pn8d3)

  no_up <- get_num_upars(pop8e)
  expect_equal(no_up, get_num_upars(pop8d3))

  par_values <- rep(0, no_up)
  lp1_8d3 <- log_prob(pop8d3$stan_fit, par_values)
  lp1_8e  <- log_prob(pop8e, par_values)
  expect_equal(lp1_8d3, lp1_8e)

  par_values <- rnorm(no_up)
  lp2_8d3 <- log_prob(pop8d3, par_values)
  lp2_8e  <- log_prob(pop8e, par_values)
  expect_equal(lp2_8d3, lp2_8e)


  # test that AR-kappa works as expected ----

  # Check that we get identical lpd
  cfg8d3 <- list(sigma_kappa_hyper = 0.01,
                 use_industry_bias = 1L,
                 use_house_bias = 1L,
                 use_design_effects = 1L,
                 g_scale = 4,
                 use_constrained_house_house_bias = 1L,
                 use_constrained_party_house_bias = 1L,
                 use_constrained_party_kappa = 1L,
                 use_random_walk_kappa = 1L)
  cfg8e <-  list(sigma_kappa_hyper = 0.01,
                 use_industry_bias = 1L,
                 use_house_bias = 1L,
                 use_design_effects = 1L,
                 g_scale = 4,
                 use_constrained_house_house_bias = 1L,
                 use_constrained_party_house_bias = 1L,
                 use_constrained_party_kappa = 1L,
                 use_ar_kappa = 1L,
                 alpha_kappa_known = 1.0)

  expect_silent(pop8d_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8e <- poll_of_polls(y = parties,
                                               model = "model8e",
                                               polls_data = spd,
                                               time_scale = time_scale,
                                               known_state = known_state,
                                               hyper_parameters = cfg8e,
                                               warmup = 0,
                                               iter = 1,
                                               chains = 1,
                                               cache_dir = NULL)
                      )
                    )
                  )
  )
  expect_silent(pop8d_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d3 <- poll_of_polls(y = parties,
                                                model = "model8d3",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = cfg8d3,
                                                warmup = 0,
                                                iter = 1,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  pn8d3 <- parameter_names(pop8d3)
  pn8e <- parameter_names(pop8e)
  # Currently these are included even though they are known
  expect_equal(pn8e[!(pn8e %in% c("alpha_kappa[1]", "alpha_beta_mu[1]", "alpha_beta_sigma[1]"))],
               pn8d3)

  no_up <- get_num_upars(pop8e)
  expect_equal(no_up, get_num_upars(pop8d3))

  par_values <- rep(0, no_up)
  lp1_8d3 <- log_prob(pop8d3, par_values)
  lp1_8e  <- log_prob(pop8e, par_values)
  expect_equal(lp1_8d3, lp1_8e)

  par_values <- rnorm(no_up)
  lp2_8d3 <- log_prob(pop8d3, par_values)
  lp2_8e  <- log_prob(pop8e, par_values)
  expect_equal(lp2_8d3, lp2_8e)

})


