context("model8e-toy")

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

  suppressMessages(suppressWarnings(
    sd <- stan_polls_data(x = spd,
                          time_scale = time_scale,
                          y_name = c("x3", "x4"),
                          model = "model8e",
                          known_state = known_state,
                          hyper_parameters = list(sigma_kappa_hyper = 0.001))
  ))

  # Simulate new with kappa = 0.05 per year
  kappa_x3_s1 <- 0.02
  kappa_x3_s2 <- 0.01
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

  # options(mc.cores = parallel::detectCores())
  # model_arguments("model8e")

  expect_silent(pop8e_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8e  <- poll_of_polls(y = parties,
                                                model = "model8e",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(use_industry_bias = 1L,
                                                                        use_ar_kappa = 1L,
                                                                        estimate_alpha_kappa = 1L,
                                                                        sigma_kappa_hyper = 0.05,
                                                                        kappa_1_sigma_hyper = 0.1,
                                                                        use_constrained_party_kappa = 1L),
                                                warmup = 1500,
                                                iter = 1750,
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

  #perc <- divergent_transitions_percentiles(pop8e)
  #xx <- apply(perc, MARGIN = 2, min)
  #sort(xx, decreasing = TRUE)[1:100]
  #xx <- apply(perc, MARGIN = 2, max)
  #sort(xx)[1:100]

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  pop <- pop8e
  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x4"
    expect_silent(
      plt <- plot(pop, party) + geom_known_state(pop, party) + geom_pop_line(pop, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_",party,".jpg")), width = 6, height = 3)
  }

  expect_silent(
    plt <- plot_parameters_areas(pop, c("alpha_kappa[1]"))
  )

  expect_silent(
    plt <- plot_parameters_areas(pop, c("sigma_kappa[1]", "sigma_kappa[2]"), c("sigma_kappa[x3]", "sigma_kappa[x4]"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_sigma_kappa.jpg")), width = 4, height = 3)
  expect_silent(
    plt <- plot_parameters_areas(pop, c("kappa[1,1]", "kappa[2,1]", "kappa[3,1]") , c("kappa[1,x3]", "kappa[2,x3]", "kappa[3,x3]"), title = paste0("Kappa"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x3.jpg")), width = 4, height = 3)
  expect_silent(
    plt <- plot_parameters_areas(pop, c("kappa[1,2]", "kappa[2,2]", "kappa[3,2]") , c("kappa[1,x4]", "kappa[2,x4]", "kappa[3,x4]"), title = paste0("Kappa"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_kappa_x4.jpg")), width = 4, height = 3)

  # plt <- plot_parameters_mcmc_scatter(pop, c("alpha_kappa[1]", "sigma_kappa[1]"))

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

  # options(mc.cores = parallel::detectCores())
  # model_arguments("model8e")

  expect_silent(pop8e_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8e <- poll_of_polls(y = parties,
                                                model = "model8e",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                slow_scales = known_state$date,
                                                hyper_parameters = list(use_house_bias = 1L,
                                                                        estimate_alpha_beta_mu = 1L,
                                                                        use_constrained_party_house_bias = 1L,
                                                                        sigma_kappa_hyper = 0.05,
                                                                        beta_mu_1_sigma_hyper = 0.02,
                                                                        beta_mu_sum_party_sigma_hyper = 0.005),
                                                warmup = 1000,
                                                iter = 1250,
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

  pop <- pop8e
  for(party in parties){
    # party <- "x4"
    expect_silent(
      plt <- plot(pop, party) + geom_known_state(pop, party) + geom_pop_line(pop, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8d_",party,".jpg")), width = 6, height = 3)
  }

  expect_silent(
    plt <- plot_parameters_areas(pop, c("alpha_beta_mu[1]"))
  )

})


test_that("test model 8e on real data", {
  skip_if_not(adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  start_date <- as.Date("2010-01-01")
  end_date <- as.Date("2021-06-30")
  parties <- c("S", "SD")
  data("swedish_elections")
  data("swedish_polls_curated")

  print_plt <- TRUE
  tmp_folder <- "tmp_figs/swe_8e"

  pd <- polls_data(y = swedish_polls_curated[, parties],
                   house = swedish_polls_curated$house,
                   publish_date = swedish_polls_curated$PublDate,
                   start_date = swedish_polls_curated$collectPeriodFrom,
                   end_date = swedish_polls_curated$collectPeriodTo,
                   n = swedish_polls_curated$n)
  pd <- subset_dates(pd, from = start_date, to = end_date)
  pd <- pd[complete_poll_info(pd)]

  house_freq <- table(houses(pd))
  pd <- pd[houses(pd) %in% names(house_freq[house_freq >= 10])]

  ed <- swedish_elections
  ed$date <- ed$PublDate

  time_scale <- "week"
  set.seed(4711)

  pd$poll_info$.house <- as.factor(as.character(pd$poll_info$.house))
  expect_message(
    sd <- stan_polls_data(x = pd,
                          time_scale = time_scale,
                          y_name = parties,
                          model = "model8e",
                          slow_scales = ed$date[-(1:4)],
                          hyper_parameters = list(use_industry_bias = 1L,
                                                  use_house_bias = 1L,
                                                  use_design_effects = 1L),
                          known_state = ed)
  )

  expect_silent(pop8e_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8e1 <-  poll_of_polls(y = parties,
                                                model = "model8e",
                                                polls_data = pd,
                                                time_scale = time_scale,
                                                known_state = ed,
                                                slow_scales = ed$date[-(1:4)],
                                                hyper_parameters = list(use_industry_bias = 1L,
                                                                        use_ar_kappa = 0L,
                                                                        use_house_bias = 1L,
                                                                        use_design_effects = 1L,
                                                                        estimate_alpha_kappa = 0L,
                                                                        sigma_kappa_hyper = 0.02,
                                                                        kappa_1_sigma_hyper = 0.02,
                                                                        use_constrained_party_kappa = 0L,
                                                                        use_constrained_house_house_bias = 1L,
                                                                        use_constrained_party_house_bias = 0L),
                                                warmup = 1000,
                                                iter = 1250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )


  expect_silent(pop8e_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8e2 <-  poll_of_polls(y = parties,
                                                model = "model8e",
                                                polls_data = pd,
                                                time_scale = time_scale,
                                                known_state = ed,
                                                slow_scales = ed$date[-(1:4)],
                                                hyper_parameters = list(use_industry_bias = 1L,
                                                                        use_latent_state_version = 1L,
                                                                        use_ar_kappa = 0L,
                                                                        use_house_bias = 1L,
                                                                        use_design_effects = 1L,
                                                                        estimate_alpha_kappa = 0L,
                                                                        sigma_kappa_hyper = 0.02,
                                                                        kappa_1_sigma_hyper = 0.02,
                                                                        use_constrained_party_kappa = 0L,
                                                                        use_constrained_house_house_bias = 1L,
                                                                        use_constrained_party_house_bias = 0L),
                                                warmup = 1000,
                                                iter = 1250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )


  expect_silent(pop8e_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8e3 <-  poll_of_polls(y = parties,
                                                 model = "model8e",
                                                 polls_data = pd,
                                                 time_scale = time_scale,
                                                 known_state = ed,
                                                 slow_scales = ed$date[-(1:4)],
                                                 hyper_parameters = list(use_industry_bias = 1L,
                                                                         use_latent_state_version = 2L,
                                                                         use_ar_kappa = 0L,
                                                                         use_house_bias = 1L,
                                                                         use_design_effects = 1L,
                                                                         estimate_alpha_kappa = 0L,
                                                                         sigma_kappa_hyper = 0.02,
                                                                         kappa_1_sigma_hyper = 0.02,
                                                                         use_constrained_party_kappa = 0L,
                                                                         use_constrained_house_house_bias = 1L,
                                                                         use_constrained_party_house_bias = 0L),
                                                 warmup = 1000,
                                                 iter = 1250,
                                                 chains = 4,
                                                 cache_dir = "tmp_cache")
                      )
                    )
                  )
  )
})
