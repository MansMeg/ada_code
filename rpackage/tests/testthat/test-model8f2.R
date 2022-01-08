context("model8f2")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)
# options(mc.cores = parallel::detectCores())


test_that("Test model 8f1 and 8f2 are identical", {
  # First we test that we get a similar result with 8f1 and 8f2
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
                            model = "model8f2",
                            known_state = known_state,
                            hyper_parameters = list(sigma_kappa_hyper = 0.01))
    )
  )

  # Simulate new with industry bias
  kappa_x3 <- 0.05
  kappa_x4 <- -0.05
  spd$y[,"x3"] <-  spd$y[,"x3"] + sd$stan_data$g_i * kappa_x3
  spd$y[,"x4"] <-  spd$y[,"x4"] + sd$stan_data$g_i * kappa_x4

  # Check that we get identical lpd
  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 1L,
               use_house_bias = 1L,
               use_design_effects = 1L,
               use_constrained_house_house_bias = 1L,
               use_constrained_party_house_bias = 1L,
               use_constrained_party_kappa = 1L,
               estimate_kappa_next = 0L,
               use_ar_kappa = 1L,
               estimate_alpha_kappa = 0L)

  expect_silent(pop8f1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8f1 <- poll_of_polls(y = parties,
                                               model = "model8f1",
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
  expect_silent(pop8f_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8f2 <- poll_of_polls(y = parties,
                                                model = "model8f2",
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

  pn8f1 <- parameter_names(pop8f1)
  pn8f2 <- parameter_names(pop8f2)
  expect_equal(pn8f1, pn8f2)

  no_up <- get_num_upars(pop8f1)
  expect_equal(no_up, get_num_upars(pop8f2))

  par_values <- rep(0, no_up)
  lp1_8f2 <- log_prob(pop8f2, par_values)
  lp1_8f1  <- log_prob(pop8f1, par_values)
  expect_equal(lp1_8f2, lp1_8f1)

  set.seed(4712)
  par_values <- rnorm(no_up)
  lp2_8f2 <- log_prob(pop8f2, par_values)
  lp2_8f1  <- log_prob(pop8f1, par_values)
  expect_equal(lp2_8f2, lp2_8f1)

})


test_that("Visualize results", {
  skip_if_not(adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")


  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(20, 40, 60, 80)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 500,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)

  # Simulate new with industry bias
  suppressMessages(
    suppressWarnings(
      sd <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8f2",
                            known_state = known_state,
                            hyper_parameters = list(sigma_kappa_hyper = 0.001))
  ))

  # Simulate new with kappa = 0.05 per year
  g_scale <- 0.383561643835616
  kappa_x3_s1 <- 0.02 * (1/g_scale)
  kappa_x3_s2 <- 0.015 * (1/g_scale)
  kappa_x3_s3 <- 0.06 * (1/g_scale)
  kappa_x3_s4 <- 0.055 * (1/g_scale)
  kappa_x3_s5 <- 0.05 * (1/g_scale)

  kappa_x4_s1 <- -0.02 * (1/g_scale)
  kappa_x4_s2 <- -0.015 * (1/g_scale)
  kappa_x4_s3 <- -0.06 * (1/g_scale)
  kappa_x4_s4 <- -0.055 * (1/g_scale)
  kappa_x4_s5 <- -0.05 * (1/g_scale)

  is_s1 <- collection_midpoint_dates(spd) <= known_state$date[1]
  is_s2 <- collection_midpoint_dates(spd) <= known_state$date[2] & collection_midpoint_dates(spd) > known_state$date[1]
  is_s3 <- collection_midpoint_dates(spd) <= known_state$date[3] & collection_midpoint_dates(spd) > known_state$date[2]
  is_s4 <- collection_midpoint_dates(spd) <= known_state$date[4] & collection_midpoint_dates(spd) > known_state$date[3]
  is_s5 <- collection_midpoint_dates(spd) > known_state$date[4]

  spd$y[is_s1,"x3"] <-  spd$y[is_s1,"x3"] + sd$stan_data$g_i[is_s1] * kappa_x3_s1
  spd$y[is_s2,"x3"] <-  spd$y[is_s2,"x3"] + sd$stan_data$g_i[is_s2] * kappa_x3_s2
  spd$y[is_s3,"x3"] <-  spd$y[is_s3,"x3"] + sd$stan_data$g_i[is_s3] * kappa_x3_s3
  spd$y[is_s4,"x3"] <-  spd$y[is_s4,"x3"] + sd$stan_data$g_i[is_s4] * kappa_x3_s4
  spd$y[is_s5,"x3"] <-  spd$y[is_s5,"x3"] + sd$stan_data$g_i[is_s5] * kappa_x3_s5

  spd$y[is_s1,"x4"] <-  spd$y[is_s1,"x4"] + sd$stan_data$g_i[is_s1] * kappa_x4_s1
  spd$y[is_s2,"x4"] <-  spd$y[is_s2,"x4"] + sd$stan_data$g_i[is_s2] * kappa_x4_s2
  spd$y[is_s3,"x4"] <-  spd$y[is_s3,"x4"] + sd$stan_data$g_i[is_s3] * kappa_x4_s3
  spd$y[is_s4,"x4"] <-  spd$y[is_s4,"x4"] + sd$stan_data$g_i[is_s4] * kappa_x4_s4
  spd$y[is_s5,"x4"] <-  spd$y[is_s5,"x4"] + sd$stan_data$g_i[is_s5] * kappa_x4_s5

  cfg <-  list(sigma_kappa_hyper = 0.02,
               use_industry_bias = 1L,
               use_house_bias = 1L,
               use_design_effects = 1L,
               use_constrained_house_house_bias = 1L,
               use_constrained_party_house_bias = 1L,
               use_constrained_party_kappa = 1L,
               estimate_kappa_next = 0L,
               use_ar_kappa = 1L,
               estimate_alpha_kappa = 1L)

  expect_silent(pop8f1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8f1 <- poll_of_polls(y = parties,
                                               model = "model8f1",
                                               polls_data = spd,
                                               time_scale = time_scale,
                                               known_state = known_state,
                                               hyper_parameters = cfg,
                                               warmup = 1000,
                                               iter = 1500,
                                               thin = 1,
                                               chains = 1,
                                               cache_dir = NULL)
                      )
                    )
                  )
  )

  pop <- pop8f1
  plt_x3_kp <- plot_parameters_areas(pop, c("kappa_pred[1,1]", "kappa_pred[2,1]", "kappa_pred[3,1]", "kappa_pred[4,1]", "kappa_pred[5,1]"))
  plt_x4_kp <- plot_parameters_areas(pop, c("kappa_pred[1,2]", "kappa_pred[2,2]", "kappa_pred[3,2]", "kappa_pred[4,2]", "kappa_pred[5,2]"))
  plt_alpha_kappa <- plot_parameters_areas(pop, c("alpha_kappa[1]"))
  plt_x3 <- plot_poll_of_polls(pop, "x3") + geom_known_state(pop, "x3") + geom_pop_line(pop, txdf[["x3"]]) + ggplot2::theme_bw()
  plt_x4 <- plot_poll_of_polls(pop, "x4") + geom_known_state(pop, "x4") + geom_pop_line(pop, txdf[["x4"]]) + ggplot2::theme_bw()

  cfg$use_t_dist_industry_bias <- 1L

  expect_silent(pop8f2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8f2 <- poll_of_polls(y = parties,
                                               model = "model8f2",
                                               polls_data = spd,
                                               time_scale = time_scale,
                                               known_state = known_state,
                                               hyper_parameters = cfg,
                                               warmup = 1000,
                                               iter = 1500,
                                               thin = 1,
                                               chains = 1,
                                               cache_dir = NULL)
                      )
                    )
                  )
  )

  pop <- pop8f2
  plt_x3_kp <- plot_parameters_areas(pop, c("kappa_pred[1,1]", "kappa_pred[2,1]", "kappa_pred[3,1]", "kappa_pred[4,1]", "kappa_pred[5,1]"))
  plt_x4_kp <- plot_parameters_areas(pop, c("kappa_pred[1,2]", "kappa_pred[2,2]", "kappa_pred[3,2]", "kappa_pred[4,2]", "kappa_pred[5,2]"))
  plt_alpha_kappa <- plot_parameters_areas(pop, c("alpha_kappa[1]"))
  plt_sigma_kappa <- plot_parameters_areas(pop, c("sigma_kappa[1]", "sigma_kappa[2]"))
  plt_nu_kappa <- plot_parameters_areas(pop, c("nu_kappa[1]"))
  plt_v_kappa <- plot_parameters_areas(pop, c("v_kappa[1]"))

  plt_x3 <- plot_poll_of_polls(pop, "x3") + geom_known_state(pop, "x3") + geom_pop_line(pop, txdf[["x3"]]) + ggplot2::theme_bw()
  plt_x4 <- plot_poll_of_polls(pop, "x4") + geom_known_state(pop, "x4") + geom_pop_line(pop, txdf[["x4"]]) + ggplot2::theme_bw()


})

