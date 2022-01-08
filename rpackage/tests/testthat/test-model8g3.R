context("model8g3")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)
# options(mc.cores = parallel::detectCores())
if(FALSE){ # For debugging
  library(testthat)
  library(adapop)
}


test_that("Test model 8g3", {
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
                            model = "model8g3",
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

  expect_silent(pop8g3_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8g3 <- poll_of_polls(y = parties,
                                                model = "model8g3",
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

  pn8g3 <- parameter_names(pop8g3)
  pn8g2 <- parameter_names(pop8g2)
  expect_equal(pn8g3, pn8g2)

  no_up3 <- get_num_upars(pop8g3)
  no_up2 <- get_num_upars(pop8g2)
  expect_equal(no_up3, no_up2)

  par_values <- rep(0, no_up2)
  lp1a <- log_prob(pop8g3, par_values)
  lp1b <- log_prob(pop8g2, par_values)
  expect_failure(expect_equal(lp1a, lp1b))
  # Note! eta_z and eta model different things in a random walk prior
  # See test case blow to elaborate
  # The non-centered version
  # eta:[[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-0.805557,-0.883529],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613],[-1.04435,-1.49613]]
  # eta_z:[[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[-0.805557,-0.883529],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[-0.238792,-0.612597],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0]]
  # sigma_x:[1,1]

  # The centered version:
  # eta:[[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[-0.805557,-0.883529],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[-1.04435,-1.49613],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0]]
  # sigma_x:[1,1]

  set.seed(4712)
  par_values <- rnorm(no_up2)
  lp2a <- log_prob(pop8g3, par_values)
  lp2b  <- log_prob(pop8g2, par_values)
  expect_failure(expect_equal(lp2a, lp2b))

  ls1 <- latent_state(pop8g3)
  ls2 <- latent_state(pop8g2)

})




test_that("Test model 8g3 without known states", {
  skip("This test suite is used for iterative testing")
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


  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 0L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 0L,
               use_softmax = 1L)

  model <- "model8g2"
  expect_message(
    suppressWarnings(
      sd8g2 <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = model,
                            known_state = known_state,
                            hyper_parameters = cfg)
    )
  )
  spts8g2 <- stan_parameters_to_store(model)
  smfp8g2 <- get_pop_stan_model_file_path(model)

  model <- "model8g3"
  expect_message(
    suppressWarnings(
      sd8g3 <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = model,
                            known_state = known_state,
                            hyper_parameters = cfg)
    )
  )
  spts8g3 <- stan_parameters_to_store(model)
  smfp8g3 <- get_pop_stan_model_file_path(model)

  expect_identical(sd8g2, sd8g3)
  expect_identical(spts8g2, spts8g3)
  expect_failure(expect_identical(smfp8g2, smfp8g3))

  m8g3 <- rstan::stan(file = smfp8g3,
                          model_name = "model8g3",
                          data = sd8g3$stan_data,
                          warmup = 0,
                          iter = 3,
                          chains = 1,
                          seed = 4711,
                          pars = spts8g3)
  # eta_z[t,] = to_row_vector(inverse(diag_pre_multiply(sigma_x, L_Omega[s_t_Omega[t]])) * to_vector((eta[t,] - eta[t-1,])));
  m8g2 <- rstan::stan(file = smfp8g2,
                      model_name = "model8g2",
                      data = sd8g3$stan_data,
                      warmup = 0,
                      iter = 3,
                      chains = 1,
                      seed = 4711,
                      pars = spts8g2)

  no_up2 <- get_num_upars(m8g2)
  no_up3 <- get_num_upars(m8g3)
  expect_identical(no_up2, no_up3)

  par_values <- rep(0, no_up2)
  lp1a <- log_prob(m8g2, par_values)
  lp1b <- log_prob(m8g3, par_values)

  # Same problem as in previous tests
  # Now, lets remove the known states

  sd8g3_noknown <- sd8g3
  #   int<lower=0, upper=T> T_known; // no of known latent states
  sd8g3_noknown$stan_data$T_known <- 0L
  # int<lower=1> x_known_t[T_known]; // time points where x is known
  sd8g3_noknown$stan_data$x_known_t <- integer()
  #   int<lower=1> x_unknown_t[T - T_known]; // time points where x is known
  sd8g3_noknown$stan_data$x_unknown_t <- 1L:sd8g3_noknown$stan_data$T
  # matrix<lower=0, upper=1>[T_known, P] x_known; // known x
  sd8g3_noknown$stan_data$x_known <- matrix(0.0, nrow = 0L, ncol = sd8g3_noknown$stan_data$P)

  # real<lower=0> g_t[T]; // years since last election
  # real<lower=0> g_i[N]; // g for each poll
  # int<lower=1, upper=T_known + 1> next_known_state_poll_index[N]; // The index of the next known state
  sd8g3_noknown$stan_data$next_known_state_poll_index <- rep(1L, sd8g3_noknown$stan_data$N)
  # int<lower=1, upper=T_known + 1> next_known_state_t_index[T]; // The index of the next known state
  sd8g3_noknown$stan_data$next_known_state_t_index <- rep(1L, sd8g3_noknown$stan_data$T)



  m8g3nk <- rstan::stan(file = smfp8g3,
                      model_name = "model8g3",
                      data = sd8g3_noknown$stan_data,
                      warmup = 0,
                      iter = 3,
                      chains = 1,
                      pars = spts8g3)
  m8g2nk <- rstan::stan(file = smfp8g2,
                      model_name = "model8g2",
                      data = sd8g3_noknown$stan_data,
                      warmup = 0,
                      iter = 3,
                      chains = 1,
                      pars = spts8g2)


  no_up2nk <- get_num_upars(m8g2nk)
  no_up3nk <- get_num_upars(m8g3nk)
  expect_identical(no_up2nk, no_up3nk)
  expect_identical(no_up2nk - 4L, no_up2)

  par_values <- rep(0, no_up2nk)
  lp1a_nk <- log_prob(m8g2nk, par_values)
  lp1b_nk <- log_prob(m8g3nk, par_values)
  expect_failure(expect_identical(m8g2nk@stanmodel, m8g3nk@stanmodel))
  expect_identical(lp1a_nk, lp1b_nk)
  # Seem to be correct!

  par_values <- rep(1, no_up2nk)
  par_values <- rnorm( no_up2nk)
  lp2a_nk <- log_prob(m8g2nk, par_values)
  lp2b_nk <- log_prob(m8g3nk, par_values)
  expect_failure(expect_identical(m8g2nk@stanmodel, m8g3nk@stanmodel))
  expect_identical(lp2a_nk, lp2b_nk)

  # Check how many parameters that are affected
  # See
  identical <- logical(no_up2nk)
  par_values <- rep(0, no_up2nk)
  for(i in 1:no_up2nk){
    tmp <- par_values
    tmp[i] <- 1.0
    lp2a_nk <- log_prob(m8g2nk, tmp)
    lp2b_nk <- log_prob(m8g3nk, tmp)
    lp2a_nk <- log_prob(m8g2, tmp)
    lp2b_nk <- log_prob(m8g3, tmp)
    identical[i] <- identical(lp2a_nk, lp2b_nk)
  }
  x <- list(stan_fit =m8g3nk)
  table(parameter_block_names(x))





  # If we have known values we get different also with the parameters set to 0




  expect_silent(pop8g3_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8g3 <- poll_of_polls(y = parties,
                                                model = "model8g3",
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

  pn8g3 <- parameter_names(pop8g3)
  pn8g2 <- parameter_names(pop8g2)
  expect_equal(pn8g3, pn8g2)

  no_up3 <- get_num_upars(pop8g3)
  no_up2 <- get_num_upars(pop8g2)
  expect_equal(no_up3, no_up2)

  par_values <- rep(0, no_up2)
  lp1a <- log_prob(pop8g3, par_values)
  lp1b <- log_prob(pop8g2, par_values)
  expect_equal(lp1a, lp1b) # Failing here: -28127.09, -26147.44

  set.seed(4712)
  par_values <- rnorm(no_up2)
  lp2a <- log_prob(pop8g3, par_values)
  lp2b  <- log_prob(pop8g2, par_values)
  expect_equal(lp2a, lp2b)

  ls1 <- latent_state(pop8g3)
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
