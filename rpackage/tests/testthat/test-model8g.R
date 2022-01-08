context("model8g")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)
# options(mc.cores = parallel::detectCores())


test_that("Test model 8g", {
  # First we test that we get a similar result with 8f1 and 8f3
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
                            model = "model8g",
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
               use_multivariate_version = 0L)

  expect_silent(pop8g1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8g1 <- poll_of_polls(y = parties,
                                               model = "model8g",
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

  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 0L,
               use_house_bias = 0L,
               use_design_effects = 0L)

  expect_silent(pop8f_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8f3 <- poll_of_polls(y = parties,
                                                model = "model8f3",
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

  pn8g1 <- parameter_names(pop8g1)
  pn8f3 <- parameter_names(pop8f3)
  expect_failure(expect_equal(pn8g1, pn8f3))

  no_up <- get_num_upars(pop8g1)
  expect_equal(no_up, get_num_upars(pop8f3))

  par_values <- rep(0, no_up)
  lp1_8f3 <- log_prob(pop8f3, par_values)
  lp1_8g1  <- log_prob(pop8g1, par_values)
  expect_equal(lp1_8f3, lp1_8g1)

  set.seed(4712)
  par_values <- rnorm(no_up)
  lp2_8f3 <- log_prob(pop8f3, par_values)
  lp2_8g1  <- log_prob(pop8g1, par_values)
  expect_equal(lp2_8f3, lp2_8g1)

  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 0L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 1L)

  expect_silent(pop8g2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8g2 <- poll_of_polls(y = parties,
                                                model = "model8g",
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

  no_up <- get_num_upars(pop8g1)
  expect_lt(no_up, get_num_upars(pop8g2))

  lp1_8g1 <- log_prob(pop8g1, rep(0, get_num_upars(pop8g1)))
  lp1_8g2 <- log_prob(pop8g2, rep(0, get_num_upars(pop8g2)))
  expect_failure(expect_equal(lp1_8g2, lp1_8g1))
})
