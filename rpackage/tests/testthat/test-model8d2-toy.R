context("model8d-toy")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)


test_that("Test model 8d2: Industry bias random walk", {
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
  kappa_x3_s2 <- 0.025
  kappa_x3_s3 <- 0.025
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

  expect_silent(pop8d2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d1 <- poll_of_polls(y = parties,
                                                model = "model8d2",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_industry_bias = 1L,
                                                                        use_random_walk_kappa = 1L,
                                                                        sigma_kappa_hyper = 0.05,
                                                                        kappa_1_sigma_hyper = 0.1),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )

  # Chain 4:  Elapsed Time: 23.066 seconds (Warm-up)
  # Chain 4:                2.12635 seconds (Sampling)
  # Chain 4:                25.1924 seconds (Total)

  # Check that it returns a pop object.
  expect_s3_class(pop8d1, "poll_of_polls")

  #perc <- divergent_transitions_percentiles(pop8d1)
  #xx <- apply(perc, MARGIN = 2, min)
  #sort(xx, decreasing = TRUE)[1:100]
  #xx <- apply(perc, MARGIN = 2, max)
  #sort(xx)[1:100]

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x4"
    expect_silent(
      plt <- plot(pop8d1, party) + geom_known_state(pop8d1, party) + geom_pop_line(pop8d1, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_",party,".jpg")), width = 6, height = 3)
  }


  expect_silent(
    plt <- plot_parameters_areas(pop8d1, c("sigma_kappa[1]", "sigma_kappa[2]"), c("sigma_kappa[x3]", "sigma_kappa[x4]"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_sigma_kappa.jpg")), width = 4, height = 3)
  expect_silent(
    plt <- plot_parameters_areas(pop8d1, c("kappa[1,1]", "kappa[2,1]", "kappa[3,1]") , c("kappa[1,x3]", "kappa[2,x3]", "kappa[3,x3]"), title = paste0("Kappa"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x3.jpg")), width = 4, height = 3)
  expect_silent(
    plt <- plot_parameters_areas(pop8d1, c("kappa[1,2]", "kappa[2,2]", "kappa[3,2]") , c("kappa[1,x4]", "kappa[2,x4]", "kappa[3,x4]"), title = paste0("Kappa"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x4.jpg")), width = 4, height = 3)

})


test_that("Test model 8d2: Industry bias random walk with g_scale", {
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
                        npolls = 150,
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
  kappa_x3_s2 <- 0.025
  kappa_x3_s3 <- 0.025
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

  expect_silent(pop8d2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d1 <- poll_of_polls(y = parties,
                                                model = "model8d2",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_industry_bias = 1L,
                                                                        use_random_walk_kappa = 1L,
                                                                        sigma_kappa_hyper = 0.05,
                                                                        kappa_1_sigma_hyper = 0.2,
                                                                        g_scale = 2),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                control = list(adapt_delta = 0.95),
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )

  # Chain 4:  Elapsed Time: 23.066 seconds (Warm-up)
  # Chain 4:                2.12635 seconds (Sampling)
  # Chain 4:                25.1924 seconds (Total)

  # Check that it returns a pop object.
  expect_s3_class(pop8d1, "poll_of_polls")

  #perc <- divergent_transitions_percentiles(pop8d1)
  #xx <- apply(perc, MARGIN = 2, min)
  #sort(xx, decreasing = TRUE)[1:100]
  #xx <- apply(perc, MARGIN = 2, max)
  #sort(xx)[1:100]

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8d1, party) + geom_known_state(pop8d1, party) + geom_pop_line(pop8d1, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_",party,".jpg")), width = 6, height = 3)
  }


  expect_silent(
    plt <- plot_parameters_areas(pop8d1, c("sigma_kappa[1]", "sigma_kappa[2]"), c("sigma_kappa[x3]", "sigma_kappa[x4]"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_sigma_kappa.jpg")), width = 4, height = 3)
  expect_silent(
    plt <- plot_parameters_areas(pop8d1, c("kappa[1,1]", "kappa[2,1]", "kappa[3,1]") , c("kappa[1,x3]", "kappa[2,x3]", "kappa[3,x3]"), title = paste0("Kappa"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x3.jpg")), width = 4, height = 3)
  expect_silent(
    plt <- plot_parameters_areas(pop8d1, c("kappa[1,2]", "kappa[2,2]", "kappa[3,2]") , c("kappa[1,x4]", "kappa[2,x4]", "kappa[3,x4]"), title = paste0("Kappa"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x4.jpg")), width = 4, height = 3)

})



test_that("Test model 8d2: Test soft constraints for Kappa", {
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
                        npolls = 150,
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
  kappa_x3_s1 <- 0.01
  kappa_x3_s2 <- 0.025
  kappa_x3_s3 <- 0.025
  kappa_x4_s1 <- -0.01
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

  expect_silent(pop8d2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d1 <- poll_of_polls(y = parties,
                                                model = "model8d2",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_industry_bias = 1L,
                                                                        # use_random_walk_kappa = 1L,
                                                                        use_constrained_party_kappa = 1L,
                                                                        sigma_kappa_hyper = 0.05,
                                                                        kappa_1_sigma_hyper = 0.2,
                                                                        kappa_sum_sigma_hyper = 0.005),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                control = list(adapt_delta = 0.8),
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )

  expect_silent(pop8d2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d2 <- poll_of_polls(y = parties,
                                                model = "model8d2",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_industry_bias = 1L,
                                                                        # use_random_walk_kappa = 1L,
                                                                        # use_constrained_party_kappa = 1L,
                                                                        sigma_kappa_hyper = 0.05,
                                                                        kappa_1_sigma_hyper = 0.2
                                                ),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                control = list(adapt_delta = 0.8),
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )
  # Chain 4:  Elapsed Time: 23.066 seconds (Warm-up)
  # Chain 4:                2.12635 seconds (Sampling)
  # Chain 4:                25.1924 seconds (Total)

  kappa_1 <- rstan::extract(pop8d1$stan_fit)
  kappa_1 <- kappa_1$kappa[,1,]
  # plot(kappa_1)
  ck1 <- cor(kappa_1)

  kappa_2 <- rstan::extract(pop8d2$stan_fit)
  kappa_2 <- kappa_2$kappa[,1,]
  # plot(kappa_2)
  ck2 <- cor(kappa_2)

  expect_gte(ck2[1,2],ck1[1,2])

  # Check that it returns a pop object.
  expect_s3_class(pop8d1, "poll_of_polls")
  expect_s3_class(pop8d2, "poll_of_polls")

})




test_that("Test model 8d2: Test soft constraints for beta_mu", {
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  print_plt <- TRUE
  tmp_folder <- "tmp_figs/8d2_toy_industry_sc"

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

  # Remove overlapping polls
  overlap_bool <- spd$poll_info$.start_date <= as.Date("2010-07-23") & spd$poll_info$.publish_date >= as.Date("2010-07-23")
  spd <- spd[!overlap_bool]
  overlap_bool <- spd$poll_info$.start_date <= as.Date("2011-07-08") & spd$poll_info$.publish_date >= as.Date("2011-07-08")
  spd <- spd[!overlap_bool]

  # Add houses
  spd$poll_info$.house <- as.factor(sample(c("A", "B"), replace = TRUE, size = length(spd$poll_info$.house)))

  # Add house bias
  bias_A_s1 <- 0.02
  bias_A_s2 <- 0.01
  bias_A_s3 <- -0.02
  bias_B_s1 <- -0.02
  bias_B_s2 <- -0.01
  bias_B_s3 <- 0.02

  bool_house_A <- spd$poll_info$.house == "A"
  bool_house_B <- spd$poll_info$.house == "B"
  bool_s1 <- collection_midpoint_dates(spd) <= as.Date("2010-07-23")
  bool_s2 <- collection_midpoint_dates(spd) > as.Date("2010-07-23") & collection_midpoint_dates(spd) <= as.Date("2011-07-08")
  bool_s3 <- collection_midpoint_dates(spd) > as.Date("2011-07-08")

  bool <- bool_house_A & bool_s1
  spd$y[bool, "x3"] <-
    spd$y[bool, "x3"] + bias_A_s1
  bool <- bool_house_A & bool_s2
  spd$y[bool, "x3"] <-
    spd$y[bool, "x3"] + bias_A_s2
  bool <- bool_house_A & bool_s3
  spd$y[bool, "x3"] <-
    spd$y[bool, "x3"] + bias_A_s3

  bool <- bool_house_A & bool_s1
  spd$y[bool, "x4"] <-
    spd$y[bool, "x4"] - bias_A_s1
  bool <- bool_house_A & bool_s2
  spd$y[bool, "x4"] <-
    spd$y[bool, "x4"] - bias_A_s2
  bool <- bool_house_A & bool_s3
  spd$y[bool, "x4"] <-
    spd$y[bool, "x4"] - bias_A_s3


  bool <- bool_house_B & bool_s1
  spd$y[bool, "x3"] <-
    spd$y[bool, "x3"] + bias_B_s1
  bool <- bool_house_B & bool_s2
  spd$y[bool, "x3"] <-
    spd$y[bool, "x3"] + bias_B_s2
  bool <- bool_house_B & bool_s3
  spd$y[bool, "x3"] <-
    spd$y[bool, "x3"] + bias_B_s3

  bool <- bool_house_B & bool_s1
  spd$y[bool, "x4"] <-
    spd$y[bool, "x4"] - bias_B_s1
  bool <- bool_house_B & bool_s2
  spd$y[bool, "x4"] <-
    spd$y[bool, "x4"] - bias_B_s2
  bool <- bool_house_B & bool_s3
  spd$y[bool, "x4"] <-
    spd$y[bool, "x4"] - bias_B_s3


  expect_silent(pop8d2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d1 <- poll_of_polls(y = parties,
                                                model = "model8d2",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_house_bias = 1L,
                                                                        # use_random_walk_kappa = 1L,
                                                                        # use_constrained_party_house_bias = 1L,
                                                                        sigma_kappa_hyper = 0.05,
                                                                        # kappa_1_sigma_hyper = 0.2,
                                                                        beta_mu_sum_party_sigma_hyper = 0.005),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                control = list(adapt_delta = 0.8),
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )
  # Chain 4:  Elapsed Time: 9.06874 seconds (Warm-up)
  # Chain 4:                0.955312 seconds (Sampling)
  # Chain 4:                10.0241 seconds (Total)

  expect_silent(pop8d2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d2 <- poll_of_polls(y = parties,
                                                model = "model8d2",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_house_bias = 1L,
                                                                        # use_random_walk_kappa = 1L,
                                                                        use_constrained_party_house_bias = 1L,
                                                                        sigma_kappa_hyper = 0.05,
                                                                        kappa_1_sigma_hyper = 0.2,
                                                                        beta_mu_sum_party_sigma_hyper = 0.005),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                control = list(adapt_delta = 0.8),
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )
  # Chain 4:  Elapsed Time: 8.58908 seconds (Warm-up)
  # Chain 4:                0.887542 seconds (Sampling)
  # Chain 4:                9.47663 seconds (Total)

  expect_silent(pop8d2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d3 <- poll_of_polls(y = parties,
                                                model = "model8d2",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_house_bias = 1L,
                                                                        # use_random_walk_kappa = 1L,
                                                                        # use_constrained_party_house_bias = 1L,
                                                                        use_constrained_house_house_bias = 1L,
                                                                        sigma_kappa_hyper = 0.05,
                                                                        kappa_1_sigma_hyper = 0.2,
                                                                        beta_mu_sum_party_sigma_hyper = 0.005,
                                                                        beta_mu_sum_house_sigma_hyper = 0.005),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                control = list(adapt_delta = 0.8),
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )
  # Chain 4:  Elapsed Time: 8.7577 seconds (Warm-up)
  # Chain 4:                0.561178 seconds (Sampling)
  # Chain 4:                9.31888 seconds (Total)

  expect_silent(pop8d2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d4 <- poll_of_polls(y = parties,
                                                model = "model8d2",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_house_bias = 1L,
                                                                        # use_random_walk_kappa = 1L,
                                                                        use_constrained_party_house_bias = 1L,
                                                                        use_constrained_house_house_bias = 1L,
                                                                        sigma_kappa_hyper = 0.05,
                                                                        kappa_1_sigma_hyper = 0.2,
                                                                        beta_mu_sum_party_sigma_hyper = 0.005,
                                                                        beta_mu_sum_house_sigma_hyper = 0.005),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                control = list(adapt_delta = 0.8),
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )

  # No constraints
  beta_mu <- rstan::extract(pop8d1$stan_fit)
  beta_mu_a <- beta_mu$beta_mu[,,1,]
  beta_mu_b <- beta_mu$beta_mu[,,2,]
  beta_mu_c <- beta_mu$beta_mu[,,,1]
  beta_mu_d <- beta_mu$beta_mu[,,,2]
  ck1a <- cor(beta_mu_a)
  ck1b <- cor(beta_mu_b)
  ck1c <- cor(beta_mu_c)
  ck1d <- cor(beta_mu_d)

  # Party constraints
  beta_mu <- rstan::extract(pop8d2$stan_fit)
  beta_mu_a <- beta_mu$beta_mu[,,1,]
  beta_mu_b <- beta_mu$beta_mu[,,2,]
  beta_mu_c <- beta_mu$beta_mu[,,,1]
  beta_mu_d <- beta_mu$beta_mu[,,,2]
  ck2a <- cor(beta_mu_a)
  ck2b <- cor(beta_mu_b)
  ck2c <- cor(beta_mu_c)
  ck2d <- cor(beta_mu_d)

  # House constraints
  beta_mu <- rstan::extract(pop8d3$stan_fit)
  beta_mu_a <- beta_mu$beta_mu[,,1,]
  beta_mu_b <- beta_mu$beta_mu[,,2,]
  beta_mu_c <- beta_mu$beta_mu[,,,1]
  beta_mu_d <- beta_mu$beta_mu[,,,2]
  ck3a <- cor(beta_mu_a)
  ck3b <- cor(beta_mu_b)
  ck3c <- cor(beta_mu_c)
  ck3d <- cor(beta_mu_d)

  # All
  beta_mu <- rstan::extract(pop8d4$stan_fit)
  beta_mu_a <- beta_mu$beta_mu[,,1,]
  beta_mu_b <- beta_mu$beta_mu[,,2,]
  beta_mu_c <- beta_mu$beta_mu[,,,1]
  beta_mu_d <- beta_mu$beta_mu[,,,2]
  ck4a <- cor(beta_mu_a)
  ck4b <- cor(beta_mu_b)
  ck4c <- cor(beta_mu_c)
  ck4d <- cor(beta_mu_d)

  expect_gte(ck1a[1,2],ck2a[1,2])
  expect_gte(ck1a[1,2],ck4a[1,2])
  expect_gte(ck1b[1,2],ck2b[1,2])
  expect_gte(ck1b[1,2],ck4b[1,2])

  expect_gte(ck1c[1,2],ck3c[1,2])
  expect_gte(ck1c[1,2],ck4c[1,2])
  expect_gte(ck1d[1,2],ck3d[1,2])
  expect_gte(ck1d[1,2],ck4d[1,2])

  # Check that it returns a pop object.
  expect_s3_class(pop8d1, "poll_of_polls")
  expect_s3_class(pop8d2, "poll_of_polls")
  expect_s3_class(pop8d3, "poll_of_polls")
  expect_s3_class(pop8d4, "poll_of_polls")

  plt <- plot_parameters_areas(pop8d1, c("beta_mu[1,1,1]", "beta_mu[1,1,2]", "beta_mu[1,2,1]", "beta_mu[1,2,2]"))
  plt <- plot_parameters_areas(pop8d2, c("beta_mu[1,1,1]", "beta_mu[1,1,2]", "beta_mu[1,2,1]", "beta_mu[1,2,2]"))
  plt <- plot_parameters_areas(pop8d3, c("beta_mu[1,1,1]", "beta_mu[1,1,2]", "beta_mu[1,2,1]", "beta_mu[1,2,2]"))
  plt <- plot_parameters_areas(pop8d4, c("beta_mu[1,1,1]", "beta_mu[1,1,2]", "beta_mu[1,2,1]", "beta_mu[1,2,2]"))

})
