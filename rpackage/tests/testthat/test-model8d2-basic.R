context("model8d3-basic")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)

test_that("Test model 8d2 and 8d3: Divergent transitions", {
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  print_plt <- TRUE
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

  suppressWarnings(
    suppressMessages(
      sd  <- stan_polls_data(x = spd,
                             time_scale = time_scale,
                             y_name = c("x3", "x4"),
                             model = "model8d2",
                             known_state = known_state)
    )
  )


  # Simulate new with kappa = 0.05 per year
  kappa_x3_s1 <- 0.0
  kappa_x3_s2 <- 0.01
  kappa_x3_s3 <- 0.01
  kappa_x4_s1 <- 0.0
  kappa_x4_s2 <- 0.0
  kappa_x4_s3 <- -0.01
  is_s1 <- collection_midpoint_dates(spd) <= known_state$date[1]
  is_s2 <- collection_midpoint_dates(spd) <= known_state$date[2] & collection_midpoint_dates(spd) > known_state$date[1]
  is_s3 <- collection_midpoint_dates(spd) > known_state$date[2]

  spd$y[is_s1,"x3"] <-  spd$y[is_s1,"x3"] + sd$stan_data$g[is_s1] * kappa_x3_s1
  spd$y[is_s2,"x3"] <-  spd$y[is_s2,"x3"] + sd$stan_data$g[is_s2] * kappa_x3_s2
  spd$y[is_s3,"x3"] <-  spd$y[is_s3,"x3"] + sd$stan_data$g[is_s3] * kappa_x3_s3
  spd$y[is_s1,"x4"] <-  spd$y[is_s1,"x4"] + sd$stan_data$g[is_s1] * kappa_x4_s1
  spd$y[is_s2,"x4"] <-  spd$y[is_s2,"x4"] + sd$stan_data$g[is_s2] * kappa_x4_s2
  spd$y[is_s3,"x4"] <-  spd$y[is_s3,"x4"] + sd$stan_data$g[is_s3] * kappa_x4_s3

  pop8d2_1 <- poll_of_polls(y = parties,
                          model = "model8d2",
                          polls_data = spd,
                          time_scale = time_scale,
                          known_state = known_state,
                          hyper_parameters = list(use_industry_bias = 1L,
                                                  use_random_walk_kappa = 0L,
                                                  sigma_kappa_hyper = 0.05,
                                                  kappa_1_sigma_hyper = 0.1),
                          warmup = 2000,
                          iter = 2250,
                          chains = 4,
                          cache_dir = "tmp_cache")

  pop8d3_1 <- poll_of_polls(y = parties,
                          model = "model8d3",
                          polls_data = spd,
                          time_scale = time_scale,
                          known_state = known_state,
                          hyper_parameters = list(use_industry_bias = 1L,
                                                  use_random_walk_kappa = 0L,
                                                  sigma_kappa_hyper = 0.05,
                                                  kappa_1_sigma_hyper = 0.1),
                          warmup = 2000,
                          iter = 2250,
                          chains = 4,
                          cache_dir = "tmp_cache")

  pop8d3_2 <- poll_of_polls(y = parties,
                            model = "model8d3",
                            polls_data = spd,
                            time_scale = time_scale,
                            known_state = known_state,
                            hyper_parameters = list(use_industry_bias = 1L,
                                                    use_random_walk_kappa = 0L,
                                                    sigma_kappa_hyper = 0.05,
                                                    kappa_1_sigma_hyper = 0.1,
                                                    use_sigma_kappa_gamma_prior = 1L,
                                                    sigma_kappa_gamma_a_hyper = 1.753,
                                                    sigma_kappa_gamma_b_hyper = 44),
                            warmup = 2000,
                            iter = 2250,
                            chains = 4,
                            cache_dir = "tmp_cache")

  pop8d3_3 <- poll_of_polls(y = parties,
                            model = "model8d3",
                            polls_data = spd,
                            time_scale = time_scale,
                            known_state = known_state,
                            hyper_parameters = list(use_industry_bias = 1L,
                                                    use_random_walk_kappa = 0L,
                                                    sigma_kappa_hyper = 0.01,
                                                    kappa_1_sigma_hyper = 0.1,
                                                    use_sigma_kappa_gamma_prior = 0L),
                            warmup = 2000,
                            iter = 2250,
                            chains = 4,
                            cache_dir = "tmp_cache")

  pop8d3_4 <- poll_of_polls(y = parties,
                            model = "model8d3",
                            polls_data = spd,
                            time_scale = time_scale,
                            known_state = known_state,
                            hyper_parameters = list(use_industry_bias = 1L,
                                                    use_random_walk_kappa = 0L,
                                                    sigma_kappa_hyper = 0.01,
                                                    kappa_1_sigma_hyper = 0.1,
                                                    use_sigma_kappa_gamma_prior = 1L,
                                                    sigma_kappa_gamma_a_hyper = 1.753,
                                                    sigma_kappa_gamma_b_hyper = 219.3),
                            warmup = 2000,
                            iter = 2250,
                            chains = 4,
                            cache_dir = "tmp_cache")

  # Chain 4:  Elapsed Time: 23.066 seconds (Warm-up)
  # Chain 4:                2.12635 seconds (Sampling)
  # Chain 4:                25.1924 seconds (Total)

  # Check that it returns a pop object.
  expect_s3_class(pop8d1, "poll_of_polls")

  pop8d1 <- pop8d3_4
  party <- "x4"
  plt <- plot(pop8d1, party) + geom_known_state(pop8d1, party) + geom_pop_line(pop8d1, txdf[[party]]) + ggplot2::theme_bw()
  party <- "x3"
  plt <- plot(pop8d1, party) + geom_known_state(pop8d1, party) + geom_pop_line(pop8d1, txdf[[party]]) + ggplot2::theme_bw()

})
