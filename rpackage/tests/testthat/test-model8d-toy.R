context("model8d-toy")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)


test_that("Test model 8d: Industry bias and house bias", {
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  print_plt <- TRUE
  tmp_folder <- "tmp_figs/8d_toy_industry_house"

  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 300,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)

  suppressWarnings(
    suppressMessages(
    sd  <- stan_polls_data(x = spd,
                           time_scale = time_scale,
                           y_name = c("x3", "x4"),
                           model = "model8d",
                           known_state = known_state)
    )
  )


  # Simulate new with kappa = 0.05 per year
  kappa_x3 <- 0.05
  kappa_x4 <- -0.05
  spd$y[,"x3"] <-  spd$y[,"x3"] + sd$stan_data$g * kappa_x3
  spd$y[,"x4"] <-  spd$y[,"x4"] + sd$stan_data$g * kappa_x4

  # Add houses
  spd$poll_info$.house <- as.factor(sample(c("A", "B"), replace = TRUE, size = length(spd$poll_info$.house)))

  # Add house bias
  bias_A_x3 <- 0.01
  bias_B_x3 <- -0.02
  bias_A_x4 <- 0.0
  bias_B_x4 <- 0.03
  spd$y[spd$poll_info$.house == "A", "x3"] <-
    spd$y[spd$poll_info$.house == "A", "x3"] + bias_A_x3
  spd$y[spd$poll_info$.house == "B", "x3"] <-
    spd$y[spd$poll_info$.house == "B", "x3"] + bias_B_x3
  spd$y[spd$poll_info$.house == "A", "x4"] <-
    spd$y[spd$poll_info$.house == "A", "x4"] + bias_A_x4
  spd$y[spd$poll_info$.house == "B", "x4"] <-
    spd$y[spd$poll_info$.house == "B", "x4"] + bias_B_x4



  expect_silent(pop8d1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d1 <- poll_of_polls(y = parties,
                                                model = "model8d",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_industry_bias = 1L,
                                                                        use_house_bias = 1L,
                                                                        sigma_kappa_hyper = 0.02),
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

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8d1, party) + geom_known_state(pop8d1, party) + geom_pop_line(pop8d1, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_",party,".jpg")), width = 6, height = 3)
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
    plt <- plot_parameters_areas(pop8d1, c("kappa[1,1]", "kappa[2,1]", "kappa[3,1]") , c("kappa[1,x3]", "kappa[2,x3]", "kappa[3,x3]"), title = paste0("Kappa (true value = ",kappa_x3,")"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x3.jpg")), width = 4, height = 3)
  expect_silent(
    plt <- plot_parameters_areas(pop8d1, c("kappa[1,2]", "kappa[2,2]", "kappa[3,2]") , c("kappa[1,x4]", "kappa[2,x4]", "kappa[3,x4]"), title = paste0("Kappa (true value = ",kappa_x4,")"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x4.jpg")), width = 4, height = 3)

  plt <- plot_parameters_areas(pop8d1, c("beta_mu[1,1,1]",
                                        "beta_mu[1,2,1]",
                                        "beta_mu[1,1,2]",
                                        "beta_mu[1,2,2]"),
                               c(paste0("beta_mu[A,x3] (truth=",bias_A_x3,")"),
                                 paste0("beta_mu[B,x3] (truth=",bias_B_x3,")"),
                                 paste0("beta_mu[A,x3] (truth=",bias_A_x4,")"),
                                 paste0("beta_mu[B,x3] (truth=",bias_B_x4,")")))

  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_house_bias.jpg")), width = 4, height = 3)


})


test_that("Test model 8d: Industry bias and election house bias", {
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  print_plt <- TRUE
  tmp_folder <- "tmp_figs/8d_toy_industry_house_elections"

  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 400,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  overlap_bool <- spd$poll_info$.start_date <= as.Date("2010-07-23") & spd$poll_info$.publish_date >= as.Date("2010-07-23")
  spd <- spd[!overlap_bool]
  overlap_bool <- spd$poll_info$.start_date <= as.Date("2011-07-08") & spd$poll_info$.publish_date >= as.Date("2011-07-08")
  spd <- spd[!overlap_bool]

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)


  if(TRUE){
    suppressWarnings(
      suppressMessages(
        sd  <- stan_polls_data(x = spd,
                               time_scale = time_scale,
                               y_name = c("x3", "x4"),
                               model = "model8d",
                               known_state = known_state)
      )
    )

    # Simulate new with kappa = 0.05 per year
    kappa_x3 <- 0.05
    kappa_x4 <- -0.05
    spd$y[,"x3"] <-  spd$y[,"x3"] + sd$stan_data$g * kappa_x3
    spd$y[,"x4"] <-  spd$y[,"x4"] + sd$stan_data$g * kappa_x4
  }

  # Add houses
  spd$poll_info$.house <- as.factor(sample(c("A", "B"), replace = TRUE, size = length(spd$poll_info$.house)))

  bool_house_A <- spd$poll_info$.house == "A"
  bool_house_B <- spd$poll_info$.house == "B"
  bool_s1 <- collection_midpoint_dates(spd) <= as.Date("2010-07-23")
  bool_s2 <- collection_midpoint_dates(spd) > as.Date("2010-07-23") & collection_midpoint_dates(spd) <= as.Date("2011-07-08")
  bool_s3 <- collection_midpoint_dates(spd) > as.Date("2011-07-08")

  if(TRUE){
    bias_A_s1 <- 0.02
    bias_A_s2 <- 0.0
    bias_A_s3 <- -0.02
    bias_B_s1 <- 0.0
    bias_B_s2 <- -0.02
    bias_B_s3 <- 0.0

    bool <- bool_house_A & bool_s1
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_A_s1
    bool <- bool_house_A & bool_s2
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_A_s2
    bool <- bool_house_A & bool_s3
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_A_s3

    bool <- bool_house_B & bool_s1
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_B_s1
    bool <- bool_house_B & bool_s2
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_B_s2
    bool <- bool_house_B & bool_s3
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_B_s3
  }

  if(FALSE){
    # Add design effects
    de_A_s1 <- 0.0
    de_A_s2 <- 1.0
    de_A_s3 <- 0.0
    de_B_s1 <- 1
    de_B_s2 <- 0.0
    de_B_s3 <- -1


    bool <- bool_house_A & bool_s1
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(bias_A_s1)^2))
    bool <- bool_house_A & bool_s2
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(bias_A_s2)^2))
    bool <- bool_house_A & bool_s3
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(bias_A_s3)^2))
    bool <- bool_house_B & bool_s1
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(bias_B_s1)^2))
    bool <- bool_house_B & bool_s2
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(bias_B_s2)^2))
    bool <- bool_house_B & bool_s3
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(bias_B_s3)^2))
  }


  expect_silent(pop8d1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d2 <- poll_of_polls(y = parties,
                                                model = "model8d",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_industry_bias = 1L,
                                                                        use_house_bias = 1L,
                                                                        sigma_kappa_hyper = 0.02),
                                                slow_scales = as.Date(c("2010-07-23", "2011-07-08")),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )

  # Chain 4:  Elapsed Time: 43.1578 seconds (Warm-up)
  # Chain 4:                5.00131 seconds (Sampling)
  # Chain 4:                48.1591 seconds (Total)

  # Check that it returns a pop object.
  expect_s3_class(pop8d2, "poll_of_polls")

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8d2, party) + geom_known_state(pop8d2, party) + geom_pop_line(pop8d2, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_",party,".jpg")), width = 6, height = 3)
    expect_silent(
      plt <- plot(pop8d2, party) + geom_known_state(pop8d2, party) + geom_pop_line(pop8d2, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_",party,".jpg")), width = 6, height = 3)
  }


  expect_silent(
    plt <- plot_parameters_areas(pop8d2, c("sigma_kappa[1]", "sigma_kappa[2]"), c("sigma_kappa[x3]", "sigma_kappa[x4]"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_sigma_kappa.jpg")), width = 4, height = 3)
  expect_silent(
    plt <- plot_parameters_areas(pop8d2, c("kappa[1,1]", "kappa[2,1]", "kappa[3,1]") , c("kappa[1,x3]", "kappa[2,x3]", "kappa[3,x3]"), title = paste0("Kappa (true value = ",kappa_x3,")"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x3.jpg")), width = 4, height = 3)
  expect_silent(
    plt <- plot_parameters_areas(pop8d2, c("kappa[1,2]", "kappa[2,2]", "kappa[3,2]") , c("kappa[1,x4]", "kappa[2,x4]", "kappa[3,x4]"), title = paste0("Kappa (true value = ",kappa_x4,")"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x4.jpg")), width = 4, height = 3)

  plt <- plot_parameters_areas(pop8d2, c("beta_mu[1,1,1]",
                                         "beta_mu[2,1,1]",
                                         "beta_mu[3,1,1]",
                                         "beta_mu[1,2,1]",
                                         "beta_mu[2,2,1]",
                                         "beta_mu[3,2,1]"),
                               c(paste0("beta_mu[s1,A,x3] (truth=",bias_A_s1,")"),
                                 paste0("beta_mu[s2,A,x3] (truth=",bias_A_s2,")"),
                                 paste0("beta_mu[s3,A,x3] (truth=",bias_A_s3,")"),
                                 paste0("beta_mu[s1,B,x3] (truth=",bias_B_s1,")"),
                                 paste0("beta_mu[s2,B,x3] (truth=",bias_B_s2,")"),
                                 paste0("beta_mu[s3,B,x3] (truth=",bias_B_s3,")")))
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, "beta_mu_bias_rwe_x3.jpg"), width = 4, height = 3)
  plt <- plot_parameters_areas(pop8d2, c("beta_mu[1,1,2]",
                                         "beta_mu[2,1,2]",
                                         "beta_mu[3,1,2]",
                                         "beta_mu[1,2,2]",
                                         "beta_mu[2,2,2]",
                                         "beta_mu[3,2,2]"),
                               c(paste0("beta_mu[s1,A,x4] (truth=",bias_A_s1,")"),
                                 paste0("beta_mu[s2,A,x4] (truth=",bias_A_s2,")"),
                                 paste0("beta_mu[s3,A,x4] (truth=",bias_A_s3,")"),
                                 paste0("beta_mu[s1,B,x4] (truth=",bias_B_s1,")"),
                                 paste0("beta_mu[s2,B,x4] (truth=",bias_B_s2,")"),
                                 paste0("beta_mu[s3,B,x4] (truth=",bias_B_s3,")")))
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, "beta_mu_bias_rwe_x4.jpg"), width = 4, height = 3)

})




test_that("Test model 8d: Industry bias and design effects elections", {
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  print_plt <- TRUE
  tmp_folder <- "tmp_figs/8d_toy_industry_design_elections"

  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 400,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  overlap_bool <- spd$poll_info$.start_date <= as.Date("2010-07-23") & spd$poll_info$.publish_date >= as.Date("2010-07-23")
  spd <- spd[!overlap_bool]
  overlap_bool <- spd$poll_info$.start_date <= as.Date("2011-07-08") & spd$poll_info$.publish_date >= as.Date("2011-07-08")
  spd <- spd[!overlap_bool]

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)


  if(TRUE){
    suppressWarnings(
      suppressMessages(
        sd  <- stan_polls_data(x = spd,
                               time_scale = time_scale,
                               y_name = c("x3", "x4"),
                               model = "model8d",
                               known_state = known_state)
      )
    )

    # Simulate new with kappa = 0.05 per year
    kappa_x3 <- 0.05
    kappa_x4 <- -0.05
    spd$y[,"x3"] <-  spd$y[,"x3"] + sd$stan_data$g * kappa_x3
    spd$y[,"x4"] <-  spd$y[,"x4"] + sd$stan_data$g * kappa_x4
  }

  # Add houses
  spd$poll_info$.house <- as.factor(sample(c("A", "B"), replace = TRUE, size = length(spd$poll_info$.house)))

  bool_house_A <- spd$poll_info$.house == "A"
  bool_house_B <- spd$poll_info$.house == "B"
  bool_s1 <- collection_midpoint_dates(spd) <= as.Date("2010-07-23")
  bool_s2 <- collection_midpoint_dates(spd) > as.Date("2010-07-23") & collection_midpoint_dates(spd) <= as.Date("2011-07-08")
  bool_s3 <- collection_midpoint_dates(spd) > as.Date("2011-07-08")

  if(FALSE){
    bias_A_s1 <- 0.02
    bias_A_s2 <- 0.0
    bias_A_s3 <- -0.02
    bias_B_s1 <- 0.0
    bias_B_s2 <- -0.02
    bias_B_s3 <- 0.0

    bool <- bool_house_A & bool_s1
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_A_s1
    bool <- bool_house_A & bool_s2
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_A_s2
    bool <- bool_house_A & bool_s3
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_A_s3

    bool <- bool_house_B & bool_s1
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_B_s1
    bool <- bool_house_B & bool_s2
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_B_s2
    bool <- bool_house_B & bool_s3
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_B_s3
  }

  if(TRUE){
    # Add design effects
    de_A_s1 <- 0.0
    de_A_s2 <- 1.0
    de_A_s3 <- 0.0
    de_B_s1 <- 1
    de_B_s2 <- 0.0
    de_B_s3 <- -1

    bool <- bool_house_A & bool_s1
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_A_s1)^2))
    bool <- bool_house_A & bool_s2
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_A_s2)^2))
    bool <- bool_house_A & bool_s3
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_A_s3)^2))
    bool <- bool_house_B & bool_s1
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_B_s1)^2))
    bool <- bool_house_B & bool_s2
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_B_s2)^2))
    bool <- bool_house_B & bool_s3
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_B_s3)^2))
  }


  expect_silent(pop8d1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d2 <- poll_of_polls(y = parties,
                                                model = "model8d",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_industry_bias = 1L,
                                                                        use_design_effects = 1L,
                                                                        sigma_kappa_hyper = 0.02),
                                                slow_scales = as.Date(c("2010-07-23", "2011-07-08")),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )

  # Chain 4:  Elapsed Time: 24.236 seconds (Warm-up)
  # Chain 4:                4.53909 seconds (Sampling)
  # Chain 4:                28.7751 seconds (Total)

  # Check that it returns a pop object.
  expect_s3_class(pop8d2, "poll_of_polls")

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8d2, party) + geom_known_state(pop8d2, party) + geom_pop_line(pop8d2, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_",party,".jpg")), width = 6, height = 3)
    expect_silent(
      plt <- plot(pop8d2, party) + geom_known_state(pop8d2, party) + geom_pop_line(pop8d2, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_",party,".jpg")), width = 6, height = 3)
  }


  expect_silent(
    plt <- plot_parameters_areas(pop8d2, c("sigma_kappa[1]", "sigma_kappa[2]"), c("sigma_kappa[x3]", "sigma_kappa[x4]"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_sigma_kappa.jpg")), width = 4, height = 3)
  expect_silent(
    plt <- plot_parameters_areas(pop8d2, c("kappa[1,1]", "kappa[2,1]", "kappa[3,1]") , c("kappa[1,x3]", "kappa[2,x3]", "kappa[3,x3]"), title = paste0("Kappa (true value = ",kappa_x3,")"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x3.jpg")), width = 4, height = 3)
  expect_silent(
    plt <- plot_parameters_areas(pop8d2, c("kappa[1,2]", "kappa[2,2]", "kappa[3,2]") , c("kappa[1,x4]", "kappa[2,x4]", "kappa[3,x4]"), title = paste0("Kappa (true value = ",kappa_x4,")"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x4.jpg")), width = 4, height = 3)

  if(FALSE){
    plt <- plot_parameters_areas(pop8d2, c("beta_mu[1,1,1]",
                                           "beta_mu[2,1,1]",
                                           "beta_mu[3,1,1]",
                                           "beta_mu[1,2,1]",
                                           "beta_mu[2,2,1]",
                                           "beta_mu[3,2,1]"),
                                 c(paste0("beta_mu[s1,A,x3] (truth=",bias_A_s1,")"),
                                   paste0("beta_mu[s2,A,x3] (truth=",bias_A_s2,")"),
                                   paste0("beta_mu[s3,A,x3] (truth=",bias_A_s3,")"),
                                   paste0("beta_mu[s1,B,x3] (truth=",bias_B_s1,")"),
                                   paste0("beta_mu[s2,B,x3] (truth=",bias_B_s2,")"),
                                   paste0("beta_mu[s3,B,x3] (truth=",bias_B_s3,")")))
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, "beta_mu_bias_rwe_x3.jpg"), width = 4, height = 3)
    plt <- plot_parameters_areas(pop8d2, c("beta_mu[1,1,2]",
                                           "beta_mu[2,1,2]",
                                           "beta_mu[3,1,2]",
                                           "beta_mu[1,2,2]",
                                           "beta_mu[2,2,2]",
                                           "beta_mu[3,2,2]"),
                                 c(paste0("beta_mu[s1,A,x4] (truth=",bias_A_s1,")"),
                                   paste0("beta_mu[s2,A,x4] (truth=",bias_A_s2,")"),
                                   paste0("beta_mu[s3,A,x4] (truth=",bias_A_s3,")"),
                                   paste0("beta_mu[s1,B,x4] (truth=",bias_B_s1,")"),
                                   paste0("beta_mu[s2,B,x4] (truth=",bias_B_s2,")"),
                                   paste0("beta_mu[s3,B,x4] (truth=",bias_B_s3,")")))
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, "beta_mu_bias_rwe_x4.jpg"), width = 4, height = 3)
  }
  if(TRUE){
    plt <- plot_parameters_areas(pop8d2, c("beta_sigma[1,1]",
                                           "beta_sigma[2,1]",
                                           "beta_sigma[3,1]",
                                           "beta_sigma[1,2]",
                                           "beta_sigma[2,2]",
                                           "beta_sigma[3,2]"),
                                 c(paste0("beta_sigma[s1,A] (truth=",de_A_s1,")"),
                                   paste0("beta_sigma[s2,A] (truth=",de_A_s2,")"),
                                   paste0("beta_sigma[s3,A] (truth=",de_A_s3,")"),
                                   paste0("beta_sigma[s1,B] (truth=",de_B_s1,")"),
                                   paste0("beta_sigma[s2,B] (truth=",de_B_s2,")"),
                                   paste0("beta_sigma[s3,B] (truth=",de_B_s3,")")))

    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_industry_de.jpg")), width = 4, height = 3)

  }

})




test_that("Test model 8d: House bias and design effects elections", {
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  print_plt <- TRUE
  tmp_folder <- "tmp_figs/8d_toy_house_design_elections"

  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 400,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  overlap_bool <- spd$poll_info$.start_date <= as.Date("2010-07-23") & spd$poll_info$.publish_date >= as.Date("2010-07-23")
  spd <- spd[!overlap_bool]
  overlap_bool <- spd$poll_info$.start_date <= as.Date("2011-07-08") & spd$poll_info$.publish_date >= as.Date("2011-07-08")
  spd <- spd[!overlap_bool]

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)


  if(FALSE){
    # Industry bias
    suppressWarnings(
      suppressMessages(
        sd  <- stan_polls_data(x = spd,
                               time_scale = time_scale,
                               y_name = c("x3", "x4"),
                               model = "model8d",
                               known_state = known_state)
      )
    )

    # Simulate new with kappa = 0.05 per year
    kappa_x3 <- 0.05
    kappa_x4 <- -0.05
    spd$y[,"x3"] <-  spd$y[,"x3"] + sd$stan_data$g * kappa_x3
    spd$y[,"x4"] <-  spd$y[,"x4"] + sd$stan_data$g * kappa_x4
  }

  # Add houses
  spd$poll_info$.house <- as.factor(sample(c("A", "B"), replace = TRUE, size = length(spd$poll_info$.house)))

  bool_house_A <- spd$poll_info$.house == "A"
  bool_house_B <- spd$poll_info$.house == "B"
  bool_s1 <- collection_midpoint_dates(spd) <= as.Date("2010-07-23")
  bool_s2 <- collection_midpoint_dates(spd) > as.Date("2010-07-23") & collection_midpoint_dates(spd) <= as.Date("2011-07-08")
  bool_s3 <- collection_midpoint_dates(spd) > as.Date("2011-07-08")

  if(TRUE){
    bias_A_s1 <- 0.02
    bias_A_s2 <- 0.0
    bias_A_s3 <- -0.02
    bias_B_s1 <- 0.0
    bias_B_s2 <- -0.02
    bias_B_s3 <- 0.0

    bool <- bool_house_A & bool_s1
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_A_s1
    bool <- bool_house_A & bool_s2
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_A_s2
    bool <- bool_house_A & bool_s3
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_A_s3

    bool <- bool_house_B & bool_s1
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_B_s1
    bool <- bool_house_B & bool_s2
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_B_s2
    bool <- bool_house_B & bool_s3
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_B_s3
  }

  if(TRUE){
    # Add design effects
    de_A_s1 <- 0.0
    de_A_s2 <- 1.0
    de_A_s3 <- 0.0
    de_B_s1 <- 1
    de_B_s2 <- 0.0
    de_B_s3 <- -1

    bool <- bool_house_A & bool_s1
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_A_s1)^2))
    bool <- bool_house_A & bool_s2
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_A_s2)^2))
    bool <- bool_house_A & bool_s3
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_A_s3)^2))
    bool <- bool_house_B & bool_s1
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_B_s1)^2))
    bool <- bool_house_B & bool_s2
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_B_s2)^2))
    bool <- bool_house_B & bool_s3
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_B_s3)^2))
  }


  expect_silent(pop8d1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d2 <- poll_of_polls(y = parties,
                                                model = "model8d",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_house_bias = 1L,
                                                                        use_design_effects = 1L,
                                                                        sigma_kappa_hyper = 0.02),
                                                slow_scales = as.Date(c("2010-07-23", "2011-07-08")),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )

  # Chain 4:  Elapsed Time: 24.236 seconds (Warm-up)
  # Chain 4:                4.53909 seconds (Sampling)
  # Chain 4:                28.7751 seconds (Total)

  # Check that it returns a pop object.
  expect_s3_class(pop8d2, "poll_of_polls")

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8d2, party) + geom_known_state(pop8d2, party) + geom_pop_line(pop8d2, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_",party,".jpg")), width = 6, height = 3)
    expect_silent(
      plt <- plot(pop8d2, party) + geom_known_state(pop8d2, party) + geom_pop_line(pop8d2, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_",party,".jpg")), width = 6, height = 3)
  }

  if(FALSE){
  expect_silent(
    plt <- plot_parameters_areas(pop8d2, c("sigma_kappa[1]", "sigma_kappa[2]"), c("sigma_kappa[x3]", "sigma_kappa[x4]"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_sigma_kappa.jpg")), width = 4, height = 3)
  expect_silent(
    plt <- plot_parameters_areas(pop8d2, c("kappa[1,1]", "kappa[2,1]", "kappa[3,1]") , c("kappa[1,x3]", "kappa[2,x3]", "kappa[3,x3]"), title = paste0("Kappa (true value = ",kappa_x3,")"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x3.jpg")), width = 4, height = 3)
  expect_silent(
    plt <- plot_parameters_areas(pop8d2, c("kappa[1,2]", "kappa[2,2]", "kappa[3,2]") , c("kappa[1,x4]", "kappa[2,x4]", "kappa[3,x4]"), title = paste0("Kappa (true value = ",kappa_x4,")"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x4.jpg")), width = 4, height = 3)
  }
  if(TRUE){
    plt <- plot_parameters_areas(pop8d2, c("beta_mu[1,1,1]",
                                           "beta_mu[2,1,1]",
                                           "beta_mu[3,1,1]",
                                           "beta_mu[1,2,1]",
                                           "beta_mu[2,2,1]",
                                           "beta_mu[3,2,1]"),
                                 c(paste0("beta_mu[s1,A,x3] (truth=",bias_A_s1,")"),
                                   paste0("beta_mu[s2,A,x3] (truth=",bias_A_s2,")"),
                                   paste0("beta_mu[s3,A,x3] (truth=",bias_A_s3,")"),
                                   paste0("beta_mu[s1,B,x3] (truth=",bias_B_s1,")"),
                                   paste0("beta_mu[s2,B,x3] (truth=",bias_B_s2,")"),
                                   paste0("beta_mu[s3,B,x3] (truth=",bias_B_s3,")")))
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, "beta_mu_bias_rwe_x3.jpg"), width = 4, height = 3)
    plt <- plot_parameters_areas(pop8d2, c("beta_mu[1,1,2]",
                                           "beta_mu[2,1,2]",
                                           "beta_mu[3,1,2]",
                                           "beta_mu[1,2,2]",
                                           "beta_mu[2,2,2]",
                                           "beta_mu[3,2,2]"),
                                 c(paste0("beta_mu[s1,A,x4] (truth=",bias_A_s1,")"),
                                   paste0("beta_mu[s2,A,x4] (truth=",bias_A_s2,")"),
                                   paste0("beta_mu[s3,A,x4] (truth=",bias_A_s3,")"),
                                   paste0("beta_mu[s1,B,x4] (truth=",bias_B_s1,")"),
                                   paste0("beta_mu[s2,B,x4] (truth=",bias_B_s2,")"),
                                   paste0("beta_mu[s3,B,x4] (truth=",bias_B_s3,")")))
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, "beta_mu_bias_rwe_x4.jpg"), width = 4, height = 3)
  }
  if(TRUE){
    plt <- plot_parameters_areas(pop8d2, c("beta_sigma[1,1]",
                                           "beta_sigma[2,1]",
                                           "beta_sigma[3,1]",
                                           "beta_sigma[1,2]",
                                           "beta_sigma[2,2]",
                                           "beta_sigma[3,2]"),
                                 c(paste0("beta_sigma[s1,A] (truth=",de_A_s1,")"),
                                   paste0("beta_sigma[s2,A] (truth=",de_A_s2,")"),
                                   paste0("beta_sigma[s3,A] (truth=",de_A_s3,")"),
                                   paste0("beta_sigma[s1,B] (truth=",de_B_s1,")"),
                                   paste0("beta_sigma[s2,B] (truth=",de_B_s2,")"),
                                   paste0("beta_sigma[s3,B] (truth=",de_B_s3,")")))

    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_industry_de.jpg")), width = 4, height = 3)

  }

})





test_that("Test model 8d: Industry bias, house bias, and design effects elections", {
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  print_plt <- TRUE
  tmp_folder <- "tmp_figs/8d_toy_industry_house_design_elections"

  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 400,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  overlap_bool <- spd$poll_info$.start_date <= as.Date("2010-07-23") & spd$poll_info$.publish_date >= as.Date("2010-07-23")
  spd <- spd[!overlap_bool]
  overlap_bool <- spd$poll_info$.start_date <= as.Date("2011-07-08") & spd$poll_info$.publish_date >= as.Date("2011-07-08")
  spd <- spd[!overlap_bool]

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)


  if(TRUE){
    # Industry bias
    suppressWarnings(
      suppressMessages(
        sd  <- stan_polls_data(x = spd,
                               time_scale = time_scale,
                               y_name = c("x3", "x4"),
                               model = "model8d",
                               known_state = known_state)
      )
    )

    # Simulate new with kappa = 0.05 per year
    kappa_x3 <- 0.05
    kappa_x4 <- -0.05
    spd$y[,"x3"] <-  spd$y[,"x3"] + sd$stan_data$g * kappa_x3
    spd$y[,"x4"] <-  spd$y[,"x4"] + sd$stan_data$g * kappa_x4
  }

  # Add houses
  spd$poll_info$.house <- as.factor(sample(c("A", "B"), replace = TRUE, size = length(spd$poll_info$.house)))

  bool_house_A <- spd$poll_info$.house == "A"
  bool_house_B <- spd$poll_info$.house == "B"
  bool_s1 <- collection_midpoint_dates(spd) <= as.Date("2010-07-23")
  bool_s2 <- collection_midpoint_dates(spd) > as.Date("2010-07-23") & collection_midpoint_dates(spd) <= as.Date("2011-07-08")
  bool_s3 <- collection_midpoint_dates(spd) > as.Date("2011-07-08")

  if(TRUE){
    bias_A_s1 <- 0.02
    bias_A_s2 <- 0.0
    bias_A_s3 <- -0.02
    bias_B_s1 <- 0.0
    bias_B_s2 <- -0.02
    bias_B_s3 <- 0.0

    bool <- bool_house_A & bool_s1
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_A_s1
    bool <- bool_house_A & bool_s2
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_A_s2
    bool <- bool_house_A & bool_s3
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_A_s3

    bool <- bool_house_B & bool_s1
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_B_s1
    bool <- bool_house_B & bool_s2
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_B_s2
    bool <- bool_house_B & bool_s3
    spd$y[bool, parties] <-
      spd$y[bool, parties] + bias_B_s3
  }

  if(TRUE){
    # Add design effects
    de_A_s1 <- 0.0
    de_A_s2 <- 1.0
    de_A_s3 <- 0.0
    de_B_s1 <- 1
    de_B_s2 <- 0.0
    de_B_s3 <- -1

    bool <- bool_house_A & bool_s1
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_A_s1)^2))
    bool <- bool_house_A & bool_s2
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_A_s2)^2))
    bool <- bool_house_A & bool_s3
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_A_s3)^2))
    bool <- bool_house_B & bool_s1
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_B_s1)^2))
    bool <- bool_house_B & bool_s2
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_B_s2)^2))
    bool <- bool_house_B & bool_s3
    spd$poll_info$.n[bool] <-
      as.integer(round(spd$poll_info$.n[bool] * exp(de_B_s3)^2))
  }


  expect_silent(pop8d1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d2 <- poll_of_polls(y = parties,
                                                model = "model8d",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_industry_bias = 1L,
                                                                        use_house_bias = 1L,
                                                                        use_design_effects = 1L,
                                                                        sigma_kappa_hyper = 0.02),
                                                slow_scales = as.Date(c("2010-07-23", "2011-07-08")),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )

  # Chain 4:  Elapsed Time: 24.236 seconds (Warm-up)
  # Chain 4:                4.53909 seconds (Sampling)
  # Chain 4:                28.7751 seconds (Total)

  # Check that it returns a pop object.
  expect_s3_class(pop8d2, "poll_of_polls")

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8d2, party) + geom_known_state(pop8d2, party) + geom_pop_line(pop8d2, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_",party,".jpg")), width = 6, height = 3)
    expect_silent(
      plt <- plot(pop8d2, party) + geom_known_state(pop8d2, party) + geom_pop_line(pop8d2, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_",party,".jpg")), width = 6, height = 3)
  }

  if(TRUE){
    expect_silent(
      plt <- plot_parameters_areas(pop8d2, c("sigma_kappa[1]", "sigma_kappa[2]"), c("sigma_kappa[x3]", "sigma_kappa[x4]"))
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_sigma_kappa.jpg")), width = 4, height = 3)
    expect_silent(
      plt <- plot_parameters_areas(pop8d2, c("kappa[1,1]", "kappa[2,1]", "kappa[3,1]") , c("kappa[1,x3]", "kappa[2,x3]", "kappa[3,x3]"), title = paste0("Kappa (true value = ",kappa_x3,")"))
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x3.jpg")), width = 4, height = 3)
    expect_silent(
      plt <- plot_parameters_areas(pop8d2, c("kappa[1,2]", "kappa[2,2]", "kappa[3,2]") , c("kappa[1,x4]", "kappa[2,x4]", "kappa[3,x4]"), title = paste0("Kappa (true value = ",kappa_x4,")"))
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x4.jpg")), width = 4, height = 3)
  }
  if(TRUE){
    plt <- plot_parameters_areas(pop8d2, c("beta_mu[1,1,1]",
                                           "beta_mu[2,1,1]",
                                           "beta_mu[3,1,1]",
                                           "beta_mu[1,2,1]",
                                           "beta_mu[2,2,1]",
                                           "beta_mu[3,2,1]"),
                                 c(paste0("beta_mu[s1,A,x3] (truth=",bias_A_s1,")"),
                                   paste0("beta_mu[s2,A,x3] (truth=",bias_A_s2,")"),
                                   paste0("beta_mu[s3,A,x3] (truth=",bias_A_s3,")"),
                                   paste0("beta_mu[s1,B,x3] (truth=",bias_B_s1,")"),
                                   paste0("beta_mu[s2,B,x3] (truth=",bias_B_s2,")"),
                                   paste0("beta_mu[s3,B,x3] (truth=",bias_B_s3,")")))
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, "beta_mu_bias_rwe_x3.jpg"), width = 4, height = 3)
    plt <- plot_parameters_areas(pop8d2, c("beta_mu[1,1,2]",
                                           "beta_mu[2,1,2]",
                                           "beta_mu[3,1,2]",
                                           "beta_mu[1,2,2]",
                                           "beta_mu[2,2,2]",
                                           "beta_mu[3,2,2]"),
                                 c(paste0("beta_mu[s1,A,x4] (truth=",bias_A_s1,")"),
                                   paste0("beta_mu[s2,A,x4] (truth=",bias_A_s2,")"),
                                   paste0("beta_mu[s3,A,x4] (truth=",bias_A_s3,")"),
                                   paste0("beta_mu[s1,B,x4] (truth=",bias_B_s1,")"),
                                   paste0("beta_mu[s2,B,x4] (truth=",bias_B_s2,")"),
                                   paste0("beta_mu[s3,B,x4] (truth=",bias_B_s3,")")))
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, "beta_mu_bias_rwe_x4.jpg"), width = 4, height = 3)
  }
  if(TRUE){
    plt <- plot_parameters_areas(pop8d2, c("beta_sigma[1,1]",
                                           "beta_sigma[2,1]",
                                           "beta_sigma[3,1]",
                                           "beta_sigma[1,2]",
                                           "beta_sigma[2,2]",
                                           "beta_sigma[3,2]"),
                                 c(paste0("beta_sigma[s1,A] (truth=",de_A_s1,")"),
                                   paste0("beta_sigma[s2,A] (truth=",de_A_s2,")"),
                                   paste0("beta_sigma[s3,A] (truth=",de_A_s3,")"),
                                   paste0("beta_sigma[s1,B] (truth=",de_B_s1,")"),
                                   paste0("beta_sigma[s2,B] (truth=",de_B_s2,")"),
                                   paste0("beta_sigma[s3,B] (truth=",de_B_s3,")")))

    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_industry_de.jpg")), width = 4, height = 3)
  }

})
