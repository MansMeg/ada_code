context("model8d")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)

test_that("Test model 8d: Industry bias", {
  # First we test that we get a similar result with 8d as with 8a4
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  print_plt <- FALSE
  tmp_folder <- "tmp_figs/toy_8d_industry"

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
      sd1 <- stan_polls_data(x = spd,
                             time_scale = time_scale,
                             y_name = c("x3", "x4"),
                             model = "model8d",
                             known_state = known_state,
                             hyper_parameters = list(sigma_kappa_hyper = 0.001))
    )
  )
  sd <- sd1

  # Simulate new with kappa = 0.02 per year
  kappa_x3 <- 0.05
  kappa_x4 <- -0.05
  spd$y[,"x3"] <-  spd$y[,"x3"] + sd$stan_data$g * kappa_x3
  spd$y[,"x4"] <-  spd$y[,"x4"] + sd$stan_data$g * kappa_x4
  # plot(spd, "x3")
  # plot(spd, "x4")

  # Test no bias
  expect_silent(pop8d_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d1 <- poll_of_polls(y = parties,
                                                model = "model8d",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(sigma_kappa_hyper = 0.02),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )

  # Chain 4:  Elapsed Time: 3.42476 seconds (Warm-up)
  # Chain 4:                0.383625 seconds (Sampling)
  # Chain 4:                3.80839 seconds (Total)

  # Assert that no industry bias is used
  industy_bias_parameters <- c("kappa[1,1]", "sigma_kappa[1]")
  expect_false(any(industy_bias_parameters %in% parameter_names(pop8d1)))

  expect_silent(pop8d_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d2 <- poll_of_polls(y = parties,
                                                model = "model8d",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(sigma_kappa_hyper = 0.02,
                                                                        use_industry_bias = 1L),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )

  # Chain 4:  Elapsed Time: 3.42476 seconds (Warm-up)
  # Chain 4:                0.383625 seconds (Sampling)
  # Chain 4:                3.80839 seconds (Total)

  # Assert that  industry bias is included
  industy_bias_parameters <- c("kappa[1,1]", "sigma_kappa[1]")
  expect_true(all(industy_bias_parameters %in% parameter_names(pop8d2)))


  # Check that it returns a pop object.
  expect_s3_class(pop8d1, "poll_of_polls")
  expect_s3_class(pop8d2, "poll_of_polls")

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8d1, party) + geom_known_state(pop8d1, party) + geom_pop_line(pop8d2, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d1_",party,".jpg")), width = 6, height = 3)
    expect_silent(
      plt <- plot(pop8d2, party) + geom_known_state(pop8d2, party) + geom_pop_line(pop8d2, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d2_",party,".jpg")), width = 6, height = 3)
  }

})


test_that("Test model 8d and 6b, and 8d and 8a4 are identical", {
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
                            model = "model8d",
                            known_state = known_state,
                            hyper_parameters = list(sigma_kappa_hyper = 0.001))
    )
  )

  # Simulate new with kappa = 0.02 per year
  kappa_x3 <- 0.05
  kappa_x4 <- -0.05
  spd$y[,"x3"] <-  spd$y[,"x3"] + sd$stan_data$g * kappa_x3
  spd$y[,"x4"] <-  spd$y[,"x4"] + sd$stan_data$g * kappa_x4

  #
  expect_silent(pop8d_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d3 <- poll_of_polls(y = parties,
                                                model = "model8d",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(sigma_kappa_hyper = 0.02,
                                                                        use_industry_bias = 1L),
                                                warmup = 0,
                                                iter = 1,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  # Check that the log_prob is identical.
  expect_silent(pop8d_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8a5 <- poll_of_polls(y = parties,
                                                model = "model8a4",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(sigma_kappa_hyper = 0.02),
                                                warmup = 0,
                                                iter = 1,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  expect_equal(length(parameter_names(pop8d3)),
               length(parameter_names(pop8a5)))

  no_up <- get_num_upars(pop8d3)
  expect_equal(no_up, get_num_upars(pop8a5))

  lp1_8d3 <- log_prob(pop8d3$stan_fit, rep(0, get_num_upars(pop8d3)))
  lp1_8a5 <- log_prob(pop8a5, rep(0, get_num_upars(pop8a5$stan_fit)))
  no_up <- get_num_upars(pop8a5)
  lp2_8d3 <- log_prob(pop8d3, rep(0.1, no_up))
  lp2_8a5 <- log_prob(pop8a5, rep(0.1, no_up))
  pars <- rnorm(no_up,0,0.1)
  lp3_8d3 <- log_prob(pop8d3, pars)
  lp3_8a5 <- log_prob(pop8a5, pars)

  expect_equal(lp1_8d3, lp1_8a5)
  expect_equal(lp2_8d3, lp2_8a5)
  expect_equal(lp3_8d3, lp3_8a5)

  expect_silent(pop8d_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d4 <- poll_of_polls(y = parties,
                                                model = "model8d",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(sigma_kappa_hyper = 0.02,
                                                                        use_industry_bias = 0L),
                                                warmup = 0,
                                                iter = 1,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )


  expect_silent(pop6b_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop6b  <- poll_of_polls(y = parties,
                                                model = "model6b",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                warmup = 0,
                                                iter = 1,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  expect_equal(length(parameter_names(pop8d4)),
               length(parameter_names(pop6b)))

  no_up <- get_num_upars(pop8d4)
  expect_equal(no_up, get_num_upars(pop6b))

  lp1_8d4 <- log_prob(pop8d4$stan_fit, rep(0, get_num_upars(pop8d4)))
  lp1_6b <- log_prob(pop6b, rep(0, get_num_upars(pop6b$stan_fit)))
  no_up <- get_num_upars(pop8d4)
  lp2_8d4 <- log_prob(pop8d4, rep(0.1, no_up))
  lp2_6b <- log_prob(pop6b, rep(0.1, no_up))
  pars <- rnorm(no_up,0,0.1)
  lp3_8d4 <- log_prob(pop8d4, pars)
  lp3_6b <- log_prob(pop6b, pars)

  expect_failure(expect_equal(lp1_8d3, lp1_8d4))

  expect_equal(lp1_8d4, lp1_6b)
  expect_equal(lp2_8d4, lp2_6b)
  expect_equal(lp3_8d4, lp3_6b)

})


test_that("Test model 8d and 8b are identical", {
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

  expect_silent(pop8d_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d  <- poll_of_polls(y = parties,
                                                model = "model8d",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_house_bias = 1L),
                                                warmup = 0,
                                                iter = 1,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  # Check that the log_prob is identical.
  expect_silent(pop8b_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8b <-  poll_of_polls(y = parties,
                                                model = "model8b",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                warmup = 0,
                                                iter = 1,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  expect_equal(length(parameter_names(pop8d)),
               length(parameter_names(pop8b)))

  no_up <- get_num_upars(pop8d)
  expect_equal(no_up, get_num_upars(pop8b))

  lp1_8d <- log_prob(pop8d, rep(0, no_up))
  lp1_8b <- log_prob(pop8b, rep(0, no_up))
  lp2_8d <- log_prob(pop8d, rep(0.1, no_up))
  lp2_8b <- log_prob(pop8b, rep(0.1, no_up))
  pars <- rnorm(no_up,0,0.1)
  lp3_8d <- log_prob(pop8d, pars)
  lp3_8b <- log_prob(pop8b, pars)

  expect_equal(lp1_8d, lp1_8b)
  expect_equal(lp2_8d, lp2_8b)
  expect_equal(lp3_8d, lp3_8b)

})



test_that("Test model 8d and 8c are identical", {
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

  expect_silent(pop8d_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d  <- poll_of_polls(y = parties,
                                                model = "model8d",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_design_effects = 1L),
                                                warmup = 0,
                                                iter = 1,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  # Check that the log_prob is identical.
  expect_silent(pop8c_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8c <-  poll_of_polls(y = parties,
                                                model = "model8c2",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                warmup = 0,
                                                iter = 1,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  expect_equal(length(parameter_names(pop8d)),
               length(parameter_names(pop8c)))

  no_up <- get_num_upars(pop8d)
  expect_equal(no_up, get_num_upars(pop8c))

  lp1_8d <- log_prob(pop8d, rep(0, no_up))
  lp1_8c <- log_prob(pop8c, rep(0, no_up))
  lp2_8d <- log_prob(pop8d, rep(0.1, no_up))
  lp2_8c <- log_prob(pop8c, rep(0.1, no_up))
  pars <- rnorm(no_up,0,0.1)
  lp3_8d <- log_prob(pop8d, pars)
  lp3_8c <- log_prob(pop8c, pars)

  expect_equal(lp1_8d, lp1_8c)
  expect_equal(lp2_8d, lp2_8c)
  expect_equal(lp3_8d, lp3_8c)

})
