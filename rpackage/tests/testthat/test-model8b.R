context("model8b")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)

test_that("test model 8b: House bias", {
  skip_if_not(adapop:::test_stan_full_on_local() | adapop:::on_github_actions())
  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  parties <- c("x3", "x4")
  print_plt <- FALSE
  tmp_folder <- "tmp_figs/toy_house_bias"

  time_scale <- "week"
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
  ltr <- setup_latent_time_ranges(x = NULL, y = parties, mtr)

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
  spd$y[spd$poll_info$.house == "B", "x4"] <-
    spd$y[spd$poll_info$.house == "B", "x4"] + bias_B_x4

  expect_message(
    sd <- stan_polls_data(x = spd,
                          time_scale = time_scale,
                          y_name = parties,
                          model = "model8b",
                          known_state = known_state)
  )

  expect_silent(pop8b_out <-
    capture.output(
      suppressWarnings(
        suppressMessages(
          pop8b <- poll_of_polls(y = parties,
                                 model = "model8b",
                                 polls_data = spd,
                                 time_scale = time_scale,
                                 known_state = known_state,
                                 warmup = 2000,
                                 iter = 2250,
                                 chains = 4,
                                 cache_dir = "tmp_cache")
          )
        )
      )
    )
  # Chain 4:  Elapsed Time: 10.9144 seconds (Warm-up)
  # Chain 4:                0.892851 seconds (Sampling)
  # Chain 4:                11.8072 seconds (Total)

  # Check that it returs a pop object.
  expect_s3_class(pop8b, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  expect_silent(ls <- latent_state(pop8b))


  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8b, party) + geom_known_state(pop8b, party) + geom_pop_line(pop8b, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8b_",party,".jpg")), width = 6, height = 3)
  }

  plt <- plot_parameters_areas(pop8b, c("beta_mu[1,1,1]",
                                        "beta_mu[1,2,1]",
                                        "beta_mu[1,1,2]",
                                        "beta_mu[1,2,2]"),
                               c(paste0("beta_mu[A,x3] (truth=",bias_A_x3,")"),
                                 paste0("beta_mu[B,x3] (truth=",bias_B_x3,")"),
                                 paste0("beta_mu[A,x3] (truth=",bias_A_x4,")"),
                                 paste0("beta_mu[B,x3] (truth=",bias_B_x4,")")))

  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8b_house_bias.jpg")), width = 4, height = 3)


  latent_time_ranges <- list("x3" = list(from = "2010-04-01"),
                             "x4" = list(to = "2011-06-01"))

  ys <- y(spd)
  ys[c(6, 19, 72),2] <- NA
  ys[c(14, 16, 87),3] <- NA
  y(spd) <- ys
  ys <- y(spd)
  ys[124:150, 2] <- NA
  ys[1:31, 3] <- NA
  y(spd) <- ys

  expect_silent(pop8b_out <-
                 capture.output(
                   suppressWarnings(
                     suppressMessages(
                       pop8b2 <- poll_of_polls(y = c("x3", "x4"),
                                               model = "model8b",
                                               polls_data = spd,
                                               time_scale = time_scale,
                                               known_state = known_state,
                                               latent_time_ranges = latent_time_ranges,
                                               warmup = 2000,
                                               iter = 2250,
                                               chains = 4,
                                               cache_dir = "tmp_cache")
                     )
                   )
                 )
  )
  # Chain 4:  Elapsed Time: 13.8498 seconds (Warm-up)
  # Chain 4:                0.694149 seconds (Sampling)
  # Chain 4:                14.544 seconds (Total)

  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8b2, party) + geom_known_state(pop8b2, party) + geom_pop_line(pop8b2, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8b2_",party,".jpg")), width = 12, height = 6)
  }

})


test_that("test model 8b1: House bias with constrained bias", {
  skip_if_not(adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())
  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")
  parties <- c("x3", "x4")

  print_plt <- FALSE
  tmp_folder <- "tmp_figs/toy_constrained_house_bias"

  time_scale <- "week"
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
  ltr <- setup_latent_time_ranges(x = NULL, y = parties, mtr)

  # Add houses
  spd$poll_info$.house <- as.factor(sample(c("A", "B"), replace = TRUE, size = length(spd$poll_info$.house)))

  # Add house bias
  bias_A_x3 <- -0.01
  bias_B_x3 <- 0.01
  bias_A_x4 <- 0.02
  bias_B_x4 <- -0.02
  spd$y[spd$poll_info$.house == "A", "x3"] <-
    spd$y[spd$poll_info$.house == "A", "x3"] + bias_A_x3
  spd$y[spd$poll_info$.house == "B", "x3"] <-
    spd$y[spd$poll_info$.house == "B", "x3"] + bias_B_x3
  spd$y[spd$poll_info$.house == "A", "x4"] <-
    spd$y[spd$poll_info$.house == "A", "x4"] + bias_A_x4
  spd$y[spd$poll_info$.house == "B", "x4"] <-
    spd$y[spd$poll_info$.house == "B", "x4"] + bias_B_x4

  expect_silent(
    sd <- stan_polls_data(x = spd,
                          time_scale = time_scale,
                          y_name = c("x3", "x4"),
                          model = "model8b1",
                          known_state = known_state,
                          slow_scales = as.Date(c("2010-06-01", "2011-01-01", "2011-06-01")))
  )

  expect_silent(
    sd <- stan_polls_data(x = spd,
                          time_scale = time_scale,
                          y_name = c("x3", "x4"),
                          model = "model8b",
                          known_state = known_state)
  )

  expect_silent(pop8b1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8b1 <- poll_of_polls(y = parties,
                                               model = "model8b1",
                                               polls_data = spd,
                                               time_scale = time_scale,
                                               known_state = known_state,
                                               warmup = 2000,
                                               iter = 2250,
                                               chains = 4,
                                               cache_dir = "tmp_cache")
                      )
                    )
                  )
  )
  # Chain 4:  Elapsed Time: 7.73559 seconds (Warm-up)
  # Chain 4:                0.702879 seconds (Sampling)
  # Chain 4:                8.43847 seconds (Total)


  # Check that it returs a pop object.
  expect_s3_class(pop8b1, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  expect_silent(ls <- latent_state(pop8b1))


  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8b1, party) + geom_known_state(pop8b1, party) + geom_pop_line(pop8b1, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8b_c_",party,".jpg")), width = 12, height = 6)
  }

  plt <- plot_parameters_areas(pop8b1, "beta_mu[1,1,1]", "beta_mu[A,x3]") + ggplot2::geom_vline(ggplot2::aes(xintercept = bias_A_x3))
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8b_c_A_x3.jpg")), width = 6, height = 4)
  plt <- plot_parameters_areas(pop8b1, "beta_mu[1,2,1]", "beta_mu[B,x3]") + ggplot2::geom_vline(ggplot2::aes(xintercept = bias_B_x3))
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8b_c_B_x3.jpg")), width = 6, height = 4)
  plt <- plot_parameters_areas(pop8b1, "beta_mu[1,1,2]", "beta_mu[A,x4]") + ggplot2::geom_vline(ggplot2::aes(xintercept = bias_A_x4))
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8b_c_A_x4.jpg")), width = 6, height = 4)
  plt <- plot_parameters_areas(pop8b1, "beta_mu[1,2,2]", "beta_mu[B,x4]") + ggplot2::geom_vline(ggplot2::aes(xintercept = bias_B_x4))
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8b_c_B_x4.jpg")), width = 6, height = 4)

})


test_that("test model 8b: House bias with random walk", {
  skip_if_not(adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())
  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  time_scale <- "week"
  parties <- c("x3", "x4")
  print_plt <- FALSE
  tmp_folder <- "tmp_figs/toy_rw_house_bias"


  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 400,
                        time_scale = time_scale,
                        start_date = "2010-01-01")
  # Remove overlapping polls
  overlap_bool <- spd$poll_info$.start_date <= as.Date("2011-01-01") & spd$poll_info$.publish_date >= as.Date("2011-01-01")
  spd <- spd[!overlap_bool]

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = parties, mtr)

  # Add houses
  spd$poll_info$.house <- as.factor(sample(c("A", "B"), replace = TRUE, size = length(spd$poll_info$.house)))

  # Add house bias
  bias_A_x3_2010 <- 0.01
  bias_A_x3_2011 <- 0.005
  bias_B_x3_2010 <- -0.02
  bias_B_x3_2011 <- -0.015
  bias_A_x4_2010 <- -0.03
  bias_A_x4_2011 <- -0.035
  bias_B_x4_2010 <- 0.02
  bias_B_x4_2011 <- 0.015

  bool_house_A <- spd$poll_info$.house == "A"
  bool_house_B <- spd$poll_info$.house == "B"
  bool_2010 <- collection_midpoint_dates(spd) < as.Date("2011-01-01")
  bool_2011 <- collection_midpoint_dates(spd) >= as.Date("2011-01-01")

  bool <- bool_house_A & bool_2010
  spd$y[bool, "x3"] <-
    spd$y[bool, "x3"] + bias_A_x3_2010
  bool <- bool_house_A & bool_2011
  spd$y[bool, "x3"] <-
    spd$y[bool, "x3"] + bias_A_x3_2011

  bool <- bool_house_B & bool_2010
  spd$y[bool, "x3"] <-
    spd$y[bool, "x3"] + bias_B_x3_2010
  bool <- bool_house_B & bool_2011
  spd$y[bool, "x3"] <-
    spd$y[bool, "x3"] + bias_B_x3_2011

  bool <- bool_house_A & bool_2010
  spd$y[bool, "x4"] <-
    spd$y[bool, "x4"] + bias_A_x4_2010
  bool <- bool_house_A & bool_2011
  spd$y[bool, "x4"] <-
    spd$y[bool, "x4"] + bias_A_x4_2011
  bool <- bool_house_B & bool_2010
  spd$y[bool, "x4"] <-
    spd$y[bool, "x4"] + bias_B_x4_2010
  bool <- bool_house_B & bool_2011
  spd$y[bool, "x4"] <-
    spd$y[bool, "x4"] + bias_B_x4_2011


  sd <- stan_polls_data(x = spd,
                        time_scale = time_scale,
                        y_name = parties,
                        model = "model8b",
                        known_state = known_state,
                        slow_scales = as.Date(c("2011-01-01")))

  expect_silent(pop8b_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8b1 <- poll_of_polls(y = parties,
                                                model = "model8b",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                slow_scales = as.Date(c("2010-12-31")),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )

  # Chain 4:  Elapsed Time: 15.2051 seconds (Warm-up)
  # Chain 4:                1.35531 seconds (Sampling)
  # Chain 4:                16.5605 seconds (Total)


  # Check that it returs a pop object.
  expect_s3_class(pop8b1, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  expect_silent(ls <- latent_state(pop8b1))

  # Check that the pop object can be plotted
  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8b1, party) + geom_known_state(pop8b1, party) + geom_pop_line(pop8b1, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8b_rw_",party,".jpg")), width = 6, height = 3)
  }


  plt <- plot_parameters_areas(pop8b1, c("beta_mu[1,1,1]",
                                        "beta_mu[2,1,1]",
                                        "beta_mu[1,2,1]",
                                        "beta_mu[2,2,1]"),
                               c(paste0("beta_mu[s1,A,x3] (truth=",bias_A_x3_2010,")"),
                                 paste0("beta_mu[s2,A,x3] (truth=",bias_A_x3_2011,")"),
                                 paste0("beta_mu[s1,B,x3] (truth=",bias_B_x3_2010,")"),
                                 paste0("beta_mu[s2,B,x3] (truth=",bias_B_x3_2011,")")))
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8b_rw_bias_x3.jpg")), width = 4, height = 3)

  plt <- plot_parameters_areas(pop8b1, c("beta_mu[1,1,2]",
                                         "beta_mu[2,1,2]",
                                         "beta_mu[1,2,2]",
                                         "beta_mu[2,2,2]"),
                               c(paste0("beta_mu[s1,A,x4] (truth=",bias_A_x4_2010,")"),
                                 paste0("beta_mu[s2,A,x4] (truth=",bias_A_x4_2011,")"),
                                 paste0("beta_mu[s1,B,x4] (truth=",bias_B_x4_2010,")"),
                                 paste0("beta_mu[s2,B,x4] (truth=",bias_B_x4_2011,")")))
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8b_rw_bias_x4.jpg")), width = 4, height = 3)

})


test_that("test model 8b: House bias with random walk and election changes", {
  skip_if_not(adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())
  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  print_plt <- FALSE
  tmp_folder <- "tmp_figs/toy_rwe_house_bias"
  parties <- c("x3", "x4")

  time_scale <- "week"
  set.seed(4711)
  true_idx <- c(30, 80)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 500,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = parties, mtr)

  # Remove overlapping polls
  overlap_bool <- spd$poll_info$.start_date <= as.Date("2010-07-23") & spd$poll_info$.publish_date >= as.Date("2010-07-23")
  spd <- spd[!overlap_bool]
  overlap_bool <- spd$poll_info$.start_date <= as.Date("2011-07-08") & spd$poll_info$.publish_date >= as.Date("2011-07-08")
  spd <- spd[!overlap_bool]

  # Add houses
  spd$poll_info$.house <- as.factor(sample(c("A", "B", "C"), replace = TRUE, size = length(spd$poll_info$.house)))

  # Add house bias
  bias_A_s1 <- 0.02
  bias_A_s2 <- 0.0
  bias_A_s3 <- -0.02
  bias_B_s1 <- 0.0
  bias_B_s2 <- 0.0
  bias_B_s3 <- -0.02
  bias_C_s1 <- 0.0
  bias_C_s2 <- 0.02
  bias_C_s3 <- 0.0

  bool_house_A <- spd$poll_info$.house == "A"
  bool_house_B <- spd$poll_info$.house == "B"
  bool_house_C <- spd$poll_info$.house == "C"
  bool_s1 <- collection_midpoint_dates(spd) <= as.Date("2010-07-23")
  bool_s2 <- collection_midpoint_dates(spd) > as.Date("2010-07-23") & collection_midpoint_dates(spd) <= as.Date("2011-07-08")
  bool_s3 <- collection_midpoint_dates(spd) > as.Date("2011-07-08")

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

  bool <- bool_house_C & bool_s1
  spd$y[bool, parties] <-
    spd$y[bool, parties] + bias_C_s1
  bool <- bool_house_C & bool_s2
  spd$y[bool, parties] <-
    spd$y[bool, parties] + bias_C_s2
  bool <- bool_house_C & bool_s3
  spd$y[bool, parties] <-
    spd$y[bool, parties] + bias_C_s3


  expect_silent(pop8b_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8b1 <- poll_of_polls(y = parties,
                                                model = "model8b",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                slow_scales = as.Date(c("2010-07-23", "2011-07-08")),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )
  # Chain 4:  Elapsed Time: 21.9915 seconds (Warm-up)
  # Chain 4:                1.16832 seconds (Sampling)
  # Chain 4:                23.1598 seconds (Total)

  # Check that it returs a pop object.
  expect_s3_class(pop8b1, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  expect_silent(ls <- latent_state(pop8b1))

  # Check that the pop object can be plotted
  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)
  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8b1, party) + geom_known_state(pop8b1, party) + geom_pop_line(pop8b1, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8b_rwe_",party,".jpg")), width = 6, height = 3)
  }

  plt <- plot_parameters_areas(pop8b1, c("beta_mu[1,1,1]",
                                         "beta_mu[2,1,1]",
                                         "beta_mu[3,1,1]",
                                         "beta_mu[1,2,1]",
                                         "beta_mu[2,2,1]",
                                         "beta_mu[3,2,1]",
                                         "beta_mu[1,3,1]",
                                         "beta_mu[2,3,1]",
                                         "beta_mu[3,3,1]"),
                               c(paste0("beta_mu[s1,A,x3] (truth=",bias_A_s1,")"),
                                 paste0("beta_mu[s2,A,x3] (truth=",bias_A_s2,")"),
                                 paste0("beta_mu[s3,A,x3] (truth=",bias_A_s3,")"),
                                 paste0("beta_mu[s1,B,x3] (truth=",bias_B_s1,")"),
                                 paste0("beta_mu[s2,B,x3] (truth=",bias_B_s2,")"),
                                 paste0("beta_mu[s3,B,x3] (truth=",bias_B_s3,")"),
                                 paste0("beta_mu[s1,C,x3] (truth=",bias_C_s1,")"),
                                 paste0("beta_mu[s2,C,x3] (truth=",bias_C_s2,")"),
                                 paste0("beta_mu[s3,C,x3] (truth=",bias_C_s3,")")))
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, "beta_mu_bias_rwe_x3.jpg"), width = 4, height = 3)
  plt <- plot_parameters_areas(pop8b1, c("beta_mu[1,1,2]",
                                         "beta_mu[2,1,2]",
                                         "beta_mu[3,1,2]",
                                         "beta_mu[1,2,2]",
                                         "beta_mu[2,2,2]",
                                         "beta_mu[3,2,2]",
                                         "beta_mu[1,3,2]",
                                         "beta_mu[2,3,2]",
                                         "beta_mu[3,3,2]"),
                               c(paste0("beta_mu[s1,A,x4] (truth=",bias_A_s1,")"),
                                 paste0("beta_mu[s2,A,x4] (truth=",bias_A_s2,")"),
                                 paste0("beta_mu[s3,A,x4] (truth=",bias_A_s3,")"),
                                 paste0("beta_mu[s1,B,x4] (truth=",bias_B_s1,")"),
                                 paste0("beta_mu[s2,B,x4] (truth=",bias_B_s2,")"),
                                 paste0("beta_mu[s3,B,x4] (truth=",bias_B_s3,")"),
                                 paste0("beta_mu[s1,C,x4] (truth=",bias_C_s1,")"),
                                 paste0("beta_mu[s2,C,x4] (truth=",bias_C_s2,")"),
                                 paste0("beta_mu[s3,C,x4] (truth=",bias_C_s3,")")))
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, "beta_mu_bias_rwe_x4.jpg"), width = 4, height = 3)

})


test_that("test model 8b: House bias in Sweden", {
  skip_if_not(adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  print_plt <- FALSE
  tmp_folder <- "tmp_figs/swe_house_bias"

  start_date <- as.Date("2010-01-01")
  end_date <- as.Date("2021-06-30")
  parties <- c("S", "M", "SD", "MP", "L", "KD", "V", "C")
  data("swedish_elections")
  data("swedish_polls_curated")


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

  ed <- swedish_elections[-1,]
  ed$date <- ed$PublDate

  time_scale <- "week"
  set.seed(4711)

  expect_error(
    sd <- stan_polls_data(x = pd,
                          time_scale = time_scale,
                          y_name = parties,
                          model = "model8b",
                          slow_scales = ed$date,
                          known_state = ed)
  )
  pd$poll_info$.house <- as.factor(as.character(pd$poll_info$.house))
  expect_silent(
    sd <- stan_polls_data(x = pd,
                          time_scale = time_scale,
                          y_name = parties,
                          model = "model8b",
                          slow_scales = ed$date,
                          known_state = ed)
  )


  expect_silent(pop8a_out <-
                 capture.output(
                   suppressWarnings(
                     suppressMessages(
                       pop8b1 <- poll_of_polls(y = parties,
                                               model = "model8b",
                                               polls_data = pd,
                                               time_scale = time_scale,
                                               known_state = ed,
                                               slow_scales = ed$date,
                                               warmup = 2000,
                                               iter = 2250,
                                               chains = 4,
                                               cache_dir = "tmp_cache")
                     )
                   )
                 )
  )
  # Chain 4:  Elapsed Time: 865.459 seconds (Warm-up)
  # Chain 4:                42.0302 seconds (Sampling)
  # Chain 4:                907.49 seconds (Total)
  # 1: There were 10 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
  # http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
  # 2: There were 1 chains where the estimated Bayesian Fraction of Missing Information was low. See

  # write_ada_json(pop8b1, "tmp_output/")
  # write_latent_mean_csv(pop8b1, "tmp_output/latent_mean_model8b.csv")

  # Check that it returs a pop object.
  expect_s3_class(pop8b1, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  expect_silent(ls <- latent_state(pop8b1))


  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  houses <- levels(pd$poll_info$.house)
  for(party in parties){
    # party <- "SD"
    expect_silent(
      plt <- plot(pop8b1, party) + geom_known_state(pop8b1, party) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("swe_dyn_hb_",party,".jpg")), width = 6, height = 3)
  }

  houses <- levels(pd$poll_info$.house)
  for(i in seq_along(parties)){
    # i <- 1
    party <- parties[i]
    for(j in seq_along(houses)){
      # j <- 1
      house <- houses[j]
      expect_silent(
        plt <- plot_parameters_areas(pop8b1,
                                     paste0("beta_mu[",1:(length(ed$date)+1),",",j,",",i,"]"),
                                     paste0("beta_mu[",c(lubridate::year(ed$date), "2022"),",",house,",",party,"]"),
                                     title = paste0("House bias of ", house, " for ", party))
      )
      if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("swe_",party,"_",house,"_hb.jpg")), width = 4, height = 3)
    }
  }

  expect_silent(
    plt <- plot_parameters_areas(pop8b1, paste0("sigma_x[", seq_along(pop8b1$y), "]"), pop8b1$y, title = "Posteriord distribution for sigma_x")
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("swe_sigma_x_house_bias.jpg")), width = 4, height = 3)

})



test_that("test model 8b: House bias in Germany", {
  skip_if_not(adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  print_plt <- FALSE
  tmp_folder <- "tmp_figs/ger_house_bias"

  start_date <- as.Date("2008-01-01")
  end_date <- as.Date("2020-01-01")
  parties <- c("CDU/CSU", "SPD", "GRUNE", "FDP", "LINKE", "AfD")
  data("german_elections")
  data("german_polls_curated")


  pd <- polls_data(y = german_polls_curated[, parties],
                   house = german_polls_curated$house,
                   publish_date = german_polls_curated$PublDate,
                   start_date = german_polls_curated$collectPeriodFrom,
                   end_date = german_polls_curated$collectPeriodTo,
                   n = german_polls_curated$n)
  pd <- subset_dates(pd, from = start_date, to = end_date)
  pd <- pd[complete_poll_info(pd)]

  house_freq <- table(houses(pd))
  pd <- pd[houses(pd) %in% names(house_freq[house_freq >= 10])]

  ed <- german_elections[-1,]
  ed$date <- ed$PublDate

  time_scale <- "week"
  set.seed(4711)

  expect_silent(
    sd <- stan_polls_data(x = pd,
                          time_scale = time_scale,
                          y_name = parties,
                          model = "model8b",
                          slow_scales = ed$date,
                          known_state = ed)
  )
  pd$poll_info$.house <- as.factor(as.character(pd$poll_info$.house))
  expect_silent(
    sd <- stan_polls_data(x = pd,
                          time_scale = time_scale,
                          y_name = parties,
                          model = "model8b",
                          slow_scales = ed$date,
                          known_state = ed)
  )


  expect_silent(pop8a_out <-
                 capture.output(
                   suppressWarnings(
                     suppressMessages(
                       pop8b1 <- poll_of_polls(y = parties,
                                               model = "model8b",
                                               polls_data = pd,
                                               time_scale = time_scale,
                                               known_state = ed,
                                               slow_scales = ed$date,
                                               warmup = 2000,
                                               iter = 2250,
                                               chains = 4,
                                               cache_dir = "tmp_cache")
                     )
                   )
                 )
  )
  # Chain 4:  Elapsed Time: 865.459 seconds (Warm-up)
  # Chain 4:                42.0302 seconds (Sampling)
  # Chain 4:                907.49 seconds (Total)
  # 1: There were 10 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
  # http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
  # 2: There were 1 chains where the estimated Bayesian Fraction of Missing Information was low. See

  # Check that it returs a pop object.
  expect_s3_class(pop8b1, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  expect_silent(ls <- latent_state(pop8b1))


  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  houses <- levels(pd$poll_info$.house)
  for(party in parties){
    # party <- "L"
    expect_silent(
      plt <- plot(pop8b1, party) + geom_known_state(pop8b1, party) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, make.names(paste0("ger_",party,"_dyn_house_bias.jpg"))), width = 6, height = 3)
  }

  houses <- levels(pd$poll_info$.house)
  for(i in seq_along(parties)){
    # i <- 1
    party <- parties[i]
    for(j in seq_along(houses)){
      # j <- 1
      house <- houses[j]
      expect_silent(
        plt <- plot_parameters_areas(pop8b1,
                                     paste0("beta_mu[",1:(length(ed$date)+1),",",j,",",i,"]"),
                                     paste0("beta_mu[",c(lubridate::year(ed$date), "2021"),",",house,",",party,"]"),
                                     title = paste0("House bias of ", house, " for ", party))
      )
      if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, make.names(paste0("ger_",party,"_",house,"_bias.jpg"))), width = 4, height = 3)
    }
  }


})
