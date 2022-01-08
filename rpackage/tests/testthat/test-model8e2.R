context("model8e2")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)


test_that("Test model 8e and 8e2 are identical", {
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
                            model = "model8e2",
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
                        pop8e2 <- poll_of_polls(y = parties,
                                                model = "model8e2",
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

  pn8e2 <- parameter_names(pop8e2)
  pn8e <- parameter_names(pop8e)
  # Currently these are included even though they are known
  expect_equal(pn8e,
               pn8e2)

  no_up <- get_num_upars(pop8e)
  expect_equal(no_up, get_num_upars(pop8e2))

  par_values <- rep(0, no_up)
  lp1_8e2 <- log_prob(pop8e2, par_values)
  lp1_8e  <- log_prob(pop8e, par_values)
  expect_equal(lp1_8e2, lp1_8e)

  set.seed(4712)
  par_values <- rnorm(no_up)
  lp2_8e2 <- log_prob(pop8e2, par_values)
  lp2_8e  <- log_prob(pop8e, par_values)
  expect_equal(lp2_8e2, lp2_8e)


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
                        pop8e2 <- poll_of_polls(y = parties,
                                                model = "model8e2",
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

  pn8e2 <- parameter_names(pop8e2)
  pn8e <- parameter_names(pop8e)
  # Currently these are included even though they are known
  expect_equal(pn8e,
               pn8e2)

  no_up <- get_num_upars(pop8e)
  expect_equal(no_up, get_num_upars(pop8e2))

  par_values <- rep(0, no_up)
  lp1_8e2 <- log_prob(pop8e2, par_values)
  lp1_8e  <- log_prob(pop8e, par_values)
  expect_equal(lp1_8e2, lp1_8e)

  par_values <- rnorm(no_up)
  lp2_8e2 <- log_prob(pop8e2, par_values)
  lp2_8e  <- log_prob(pop8e, par_values)
  expect_equal(lp2_8e2, lp2_8e)


})


test_that("Test model 8d2: Industry bias random walk", {
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())
  # options(mc.cores = parallel::detectCores())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  print_plt <- FALSE
  tmp_folder <- "tmp_figs/8d2_toy_industry_rw"

  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 500,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)

  ma <- model_arguments("model8e2")
  sd  <- stan_polls_data(x = spd,
                         time_scale = time_scale,
                         y_name = c("x3", "x4"),
                         model = "model8e2",
                         known_state = known_state)

  # Simulate new with kappa = 0.05 per year
  kappa_x3_s1 <- 0.02
  kappa_x3_s2 <- 0.01
  kappa_x3_s3 <- 0.01
  kappa_x4_s1 <- -0.05
  kappa_x4_s2 <- -0.025
  kappa_x4_s3 <- -0.025
  is_s1 <- collection_midpoint_dates(spd) <= known_state$date[1]
  is_s2 <- collection_midpoint_dates(spd) <= known_state$date[2] & collection_midpoint_dates(spd) > known_state$date[1]
  is_s3 <- collection_midpoint_dates(spd) > known_state$date[2]

  spd$y[is_s1,"x3"] <-  spd$y[is_s1,"x3"] + sd$stan_data$g[is_s1] * kappa_x3_s1
  spd$y[is_s2,"x3"] <-  spd$y[is_s2,"x3"] + sd$stan_data$g[is_s2] * kappa_x3_s2
  spd$y[is_s3,"x3"] <-  spd$y[is_s3,"x3"] + sd$stan_data$g[is_s3] * kappa_x3_s3
  spd$y[is_s1,"x4"] <-  spd$y[is_s1,"x4"] + sd$stan_data$g[is_s1] * kappa_x4_s1
  spd$y[is_s2,"x4"] <-  spd$y[is_s2,"x4"] + sd$stan_data$g[is_s2] * kappa_x4_s2
  spd$y[is_s3,"x4"] <-  spd$y[is_s3,"x4"] + sd$stan_data$g[is_s3] * kappa_x4_s3

  # options(mc.cores = parallel::detectCores())
  # model_arguments("model8e")

  expect_silent(pop8e_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8e  <- poll_of_polls(y = parties,
                                                model = "model8e2",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_industry_bias = 1L,
                                                                        use_ar_kappa = 1L,
                                                                        estimate_alpha_kappa = 1L,
                                                                        sigma_kappa_hyper = 0.05,
                                                                        kappa_1_sigma_hyper = 0.1,
                                                                        use_constrained_party_kappa = 1L),
                                                warmup = 750,
                                                iter = 1000,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )

  # Chain 4:  Elapsed Time: 23.066 seconds (Warm-up)
  # Chain 4:                2.12635 seconds (Sampling)
  # Chain 4:                25.1924 seconds (Total)

  expect_silent(pop8e_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8e2  <- poll_of_polls(y = parties,
                                                model = "model8e2",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_industry_bias = 1L,
                                                                        use_ar_kappa = 1L,
                                                                        estimate_alpha_kappa = 1L,
                                                                        sigma_kappa_hyper = 0.03,
                                                                        kappa_1_sigma_hyper = 0.03,
                                                                        estimate_kappa_next = 0L,
                                                                        use_constrained_party_kappa = 1L),
                                                warmup = 750,
                                                iter = 1000,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )


  # Check that it returns a pop object.
  expect_s3_class(pop8d1, "poll_of_polls")

  #perc <- divergent_transitions_percentiles(pop8e)
  #xx <- apply(perc, MARGIN = 2, min)
  #sort(xx, decreasing = TRUE)[1:100]
  #xx <- apply(perc, MARGIN = 2, max)
  #sort(xx)[1:100]

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  party <- "x3"
  plt8ex3 <- plot(pop8e, party) + geom_known_state(pop8e, party) + geom_pop_line(pop8e, txdf[[party]]) + ggplot2::theme_bw()
  party <- "x4"
  plt8ex4 <- plot(pop8e, party) + geom_known_state(pop8e, party) + geom_pop_line(pop8e, txdf[[party]]) + ggplot2::theme_bw()

  party <- "x3"
  plt8e2x3 <- plot(pop8e2, party) + geom_known_state(pop8e2, party) + geom_pop_line(pop8e2, txdf[[party]]) + ggplot2::theme_bw()
  pop <- pop8e2; party <- "x4"
  plt8ex4 <- plot(pop8e2, party) + geom_known_state(pop8e2, party) + geom_pop_line(pop8e2, txdf[[party]]) + ggplot2::theme_bw()


  pltak1 <- plot_parameters_areas(pop8e, c("alpha_kappa[1]"))
  pltak2 <- plot_parameters_areas(pop8e2, c("alpha_kappa[1]"))


  plt8esk <- plot_parameters_areas(pop8e, c("sigma_kappa[1]", "sigma_kappa[2]"), c("sigma_kappa[x3]", "sigma_kappa[x4]"))
  plt82esk <- plot_parameters_areas(pop8e2, c("sigma_kappa[1]", "sigma_kappa[2]"), c("sigma_kappa[x3]", "sigma_kappa[x4]"))


  plt8ek <- plot_parameters_areas(pop8e, c("kappa[1,1]", "kappa[2,1]", "kappa[3,1]") , c("kappa[1,x3]", "kappa[2,x3]", "kappa[3,x3]"), title = paste0("Kappa"))
  plt8e2k <- plot_parameters_areas(pop8e2, c("kappa[1,1]", "kappa[2,1]") , c("kappa[1,x3]", "kappa[2,x3]"), title = paste0("Kappa"))

  kappa <- rstan::extract(pop8e2$stan_fit)$kappa
  kappa3 <- as.vector(kappa[,3,, drop= TRUE])
  expect_equal(kappa3,rep(0, length(kappa3)), tolerance = 0.0000001)

})


