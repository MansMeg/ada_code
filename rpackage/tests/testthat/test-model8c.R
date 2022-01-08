context("model8c")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)

test_that("test model 8c: Design effects", {
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  parties <- c("x3", "x4")
  print_plt <- FALSE
  tmp_folder <- "tmp_figs/toy_design_effects"

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
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)

  # Add houses
  spd$poll_info$.house <- as.factor(sample(c("A", "B"), replace = TRUE, size = length(spd$poll_info$.house)))

  # Add house bias
  bias_A <- -1
  bias_B <- 1
  spd$poll_info$.n[spd$poll_info$.house == "A"] <-
    as.integer(round(spd$poll_info$.n[spd$poll_info$.house == "A"] * exp(bias_A)^2))
  spd$poll_info$.n[spd$poll_info$.house == "B"] <-
    as.integer(round(spd$poll_info$.n[spd$poll_info$.house == "B"] * exp(bias_B)^2))

  expect_message(
    sd <- stan_polls_data(x = spd,
                          time_scale = time_scale,
                          y_name = c("x3", "x4"),
                          model = "model8c",
                          known_state = known_state,
                          slow_scales = as.Date(c( "2010-06-01", "2011-01-01", "2011-06-01")))
  )

  expect_message(
    sd <- stan_polls_data(x = spd,
                          time_scale = time_scale,
                          y_name = c("x3", "x4"),
                          model = "model8c",
                          known_state = known_state)
  )


  expect_silent(pop8c_out <-
    capture.output(
      suppressWarnings(
        suppressMessages(
          pop8c <- poll_of_polls(y = c("x3", "x4"),
                                 model = "model8c",
                                 polls_data = spd,
                                 time_scale = time_scale,
                                 known_state = known_state,
#                                 slow_scales = as.Date(c("2010-01-01", "2011-01-01")),
                                 warmup = 2000,
                                 iter = 2250,
                                 chains = 4,
                                 cache_dir = "tmp_cache")
          )
        )
      )
    )
  # Chain 4:  Elapsed Time: 5.25047 seconds (Warm-up)
  # Chain 4:                0.548865 seconds (Sampling)
  # Chain 4:                5.79933 seconds (Total)

  # Check that it returs a pop object.
  expect_s3_class(pop8c, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  expect_silent(ls <- latent_state(pop8c))

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)
  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8c, "x3") + geom_pop_line(pop8c, txdf[[party]]) + geom_known_state(known_state, "x3") + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8c_",party,".jpg")), width = 12, height = 6)
  }

  plt <- plot_parameters_areas(pop8c, c("beta_sigma[1,1,1]",
                                        "beta_sigma[1,2,1]",
                                        "beta_sigma[1,1,2]",
                                        "beta_sigma[1,2,2]"),
                               c(paste0("beta_sigma[A,x3] (truth=",bias_A,")"),
                                 paste0("beta_sigma[B,x3] (truth=",bias_B,")"),
                                 paste0("beta_sigma[A,x4] (truth=",bias_A,")"),
                                 paste0("beta_sigma[B,x4] (truth=",bias_B,")")))
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8c_de.jpg")), width = 6, height = 4)

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

  expect_silent(pop8c_out <-
                 capture.output(
                   suppressWarnings(
                     suppressMessages(
                       pop8c2 <- poll_of_polls(y = c("x3", "x4"),
                                               model = "model8c",
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
  # Chain 4:  Elapsed Time: 10.8304 seconds (Warm-up)
  # Chain 4:                0.740357 seconds (Sampling)
  # Chain 4:                11.5707 seconds (Total)

  # Check that it returs a pop object.
  expect_s3_class(pop8c2, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  expect_silent(ls <- latent_state(pop8c2))

})




test_that("test model 8c2: Common Design effects", {
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  parties <- c("x3", "x4")
  tmp_folder <- "tmp_figs/toy_common_design_effect"
  print_plt <- FALSE

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
  bias_A <- -1
  bias_B <- 0.5
  spd$poll_info$.n[spd$poll_info$.house == "A"] <-
    as.integer(round(spd$poll_info$.n[spd$poll_info$.house == "A"] * exp(bias_A)^2))
  spd$poll_info$.n[spd$poll_info$.house == "B"] <-
    as.integer(round(spd$poll_info$.n[spd$poll_info$.house == "B"] * exp(bias_B)^2))


  expect_silent(pop8c2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8c2 <- poll_of_polls(y = parties,
                                               model = "model8c2",
                                               polls_data = spd,
                                               time_scale = time_scale,
                                               known_state = known_state,
                                               #                                 slow_scales = as.Date(c("2010-01-01", "2011-01-01")),
                                               warmup = 2000,
                                               iter = 2250,
                                               chains = 4,
                                               cache_dir = "tmp_cache")
                      )
                    )
                  )
  )
  # Chain 4:  Elapsed Time: 5.46974 seconds (Warm-up)
  # Chain 4:                0.61762 seconds (Sampling)
  # Chain 4:                6.08736 seconds (Total)

  # Check that it returs a pop object.
  expect_s3_class(pop8c2, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  expect_silent(ls <- latent_state(pop8c2))

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8c2, "x3") + geom_pop_line(pop8c2, txdf[[party]]) + geom_known_state(known_state, "x3") + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8c2_dyn_",party,".jpg")), width = 12, height = 6)
  }

  plt <- plot_parameters_areas(pop8c2, c("beta_sigma[1,1]",
                                        "beta_sigma[1,2]"),
                               c(paste0("beta_sigma[A] (truth=",bias_A,")"),
                                 paste0("beta_sigma[B] (truth=",bias_B,")")))
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8c2_de.jpg")), width = 6, height = 4)

})



test_that("test model 8c2: Common dynamic Design effects elections", {
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  parties <- c("x3", "x4")
  tmp_folder <- "tmp_figs/toy_common_design_effect"
  print_plt <- FALSE

  time_scale <- "week"
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
  ltr <- setup_latent_time_ranges(x = NULL, y = parties, mtr)

  # Add houses
  spd$poll_info$.house <- as.factor(sample(c("A", "B"), replace = TRUE, size = length(spd$poll_info$.house)))

  # Remove overlapping polls
  overlap_bool <- spd$poll_info$.start_date <= as.Date("2010-07-23") & spd$poll_info$.publish_date >= as.Date("2010-07-23")
  spd <- spd[!overlap_bool]
  overlap_bool <- spd$poll_info$.start_date <= as.Date("2011-07-08") & spd$poll_info$.publish_date >= as.Date("2011-07-08")
  spd <- spd[!overlap_bool]

  # Add design effects
  bias_A_s1 <- 0.0
  bias_A_s2 <- 1.0
  bias_A_s3 <- 0.0
  bias_B_s1 <- 1
  bias_B_s2 <- 0.0
  bias_B_s3 <- -1

  bool_house_A <- spd$poll_info$.house == "A"
  bool_house_B <- spd$poll_info$.house == "B"
  bool_s1 <- spd$poll_info$.publish_date <= as.Date("2010-07-23")
  bool_s2 <- spd$poll_info$.publish_date > as.Date("2010-07-23") & spd$poll_info$.publish_date <= as.Date("2011-07-08")
  bool_s3 <- spd$poll_info$.publish_date > as.Date("2011-07-08")

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


  expect_silent(pop8c2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8c2 <- poll_of_polls(y = parties,
                                                model = "model8c2",
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
  # Chain 4:  Elapsed Time: 5.77416 seconds (Warm-up)
  # Chain 4:                0.737374 seconds (Sampling)
  # Chain 4:                6.51153 seconds (Total)

  # Check that it returs a pop object.
  expect_s3_class(pop8c2, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  expect_silent(ls <- latent_state(pop8c2))

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8c2, "x3") + geom_pop_line(pop8c2, txdf[[party]]) + geom_known_state(known_state, "x3") + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8c2_dyne_",party,".jpg")), width = 6, height = 3)
  }

  plt <- plot_parameters_areas(pop8c2, c("beta_sigma[1,1]",
                                         "beta_sigma[2,1]",
                                         "beta_sigma[3,1]",
                                         "beta_sigma[1,2]",
                                         "beta_sigma[2,2]",
                                         "beta_sigma[3,2]"),
                               c(paste0("beta_sigma[s1,A] (truth=",bias_A_s1,")"),
                                 paste0("beta_sigma[s2,A] (truth=",bias_A_s2,")"),
                                 paste0("beta_sigma[s3,A] (truth=",bias_A_s3,")"),
                                 paste0("beta_sigma[s1,B] (truth=",bias_B_s1,")"),
                                 paste0("beta_sigma[s2,B] (truth=",bias_B_s2,")"),
                                 paste0("beta_sigma[s3,B] (truth=",bias_B_s3,")")))

  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8c2_dyne_de.jpg")), width = 4, height = 3)

})




test_that("test model 8c: Design effect in Sweden", {
  skip_if_not(adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  start_date <- as.Date("2010-01-01")
  end_date <- as.Date("2021-06-30")
  parties <- c("S", "M", "SD", "MP", "L", "KD", "V", "C")
  data("swedish_elections")
  data("swedish_polls_curated")

  print_plt <- FALSE
  tmp_folder <- "tmp_figs/swe_design_effects"

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
                          model = "model8c",
                          slow_scales = ed$date,
                          known_state = ed)
  )
  pd$poll_info$.house <- as.factor(as.character(pd$poll_info$.house))
  expect_silent(
    sd <- stan_polls_data(x = pd,
                          time_scale = time_scale,
                          y_name = parties,
                          model = "model8c",
                          slow_scales = ed$date,
                          known_state = ed)
  )
  expect_silent(
    sd <- stan_polls_data(x = pd,
                          time_scale = time_scale,
                          y_name = parties,
                          model = "model8c2",
                          slow_scales = ed$date,
                          known_state = ed)
  )


  expect_silent(pop8c_out <-
                 capture.output(
                   suppressWarnings(
                     suppressMessages(
                       pop8c <-  poll_of_polls(y = parties,
                                               model = "model8c",
                                               polls_data = pd,
                                               time_scale = time_scale,
                                               known_state = ed,
#                                               slow_scales = ed$date,
                                               warmup = 2000,
                                               iter = 2250,
                                               chains = 4,
                                               cache_dir = "tmp_cache")
                     )
                   )
                 )
  )



  # Check that it returs a pop object.
  expect_s3_class(pop8c, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  expect_silent(ls <- latent_state(pop8c))

  expect_silent(pop8c_out <-
                 capture.output(
                   suppressWarnings(
                     suppressMessages(
                       pop8c2 <-  poll_of_polls(y = parties,
                                               model = "model8c",
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
  # 1: There were 1 chains where the estimated Bayesian Fraction of Missing Information was low. See
  # Chain 4:  Elapsed Time: 652.209 seconds (Warm-up)
  # Chain 4:                56.5076 seconds (Sampling)
  # Chain 4:                708.717 seconds (Total)

  expect_silent(pop8c_out <-
                 capture.output(
                   suppressWarnings(
                     suppressMessages(
                       pop8c3 <-  poll_of_polls(y = parties,
                                               model = "model8c2",
                                               polls_data = pd,
                                               time_scale = time_scale,
                                               known_state = ed,
                                               #                                               slow_scales = ed$date,
                                               warmup = 2000,
                                               iter = 2250,
                                               chains = 4,
                                               cache_dir = "tmp_cache")
                     )
                   )
                 )
  )
  # Chain 4:  Elapsed Time: 462.103 seconds (Warm-up)
  # Chain 4:                42.8604 seconds (Sampling)
  # Chain 4:                504.964 seconds (Total)


  expect_silent(pop8c_out <-
                 capture.output(
                   suppressWarnings(
                     suppressMessages(
                       pop8c4 <-  poll_of_polls(y = parties,
                                               model = "model8c2",
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

  # write_ada_json(pop8c4, "tmp_output/")
  # write_latent_mean_csv(pop8c4, "tmp_output/latent_mean_model8c2.csv")

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "L"
    expect_silent(
      plt <- plot(pop8c4, party) + geom_known_state(pop8c4, party) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("swe_8c2_",party,"_de.jpg")), width = 6, height = 3)
  }

  houses <- levels(pd$poll_info$.house)
  for(j in seq_along(houses)){
    # j <- 1
    house <- houses[j]
    expect_silent(
      plt <- plot_parameters_areas(pop8c4,
                                   paste0("beta_sigma[",1:(length(ed$date)+1),",",j,"]"),
                                   paste0("beta_sigma[",c(lubridate::year(ed$date), "2022"),",",house,"]"),
                                   title = paste0("Design effect of ", house))
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("swe_",house,"_design_effect.jpg")), width = 6, height = 4)
  }

  expect_silent(
    plt <- plot_parameters_areas(pop8c4, paste0("sigma_x[", seq_along(pop8c4$y), "]"), pop8c4$y, title = "Posteriord distribution for sigma_x")
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("swe_sigma_x_8c2.jpg")), width = 4, height = 3)

})



test_that("test model 8c: Design effect in Germany", {
  skip_if_not(adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())


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

  print_plt <- FALSE
  tmp_folder <- "tmp_figs/ger_design_effects"
  time_scale <- "week"
  set.seed(4711)

  expect_silent(
    sd <- stan_polls_data(x = pd,
                          time_scale = time_scale,
                          y_name = parties,
                          model = "model8c",
                          slow_scales = ed$date,
                          known_state = ed)
  )
  pd$poll_info$.house <- as.factor(as.character(pd$poll_info$.house))


  expect_silent(pop8c_out <-
                 capture.output(
                   suppressWarnings(
                     suppressMessages(
                       pop8c4 <-  poll_of_polls(y = parties,
                                                model = "model8c2",
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


  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "L"
    expect_silent(
      plt <- plot(pop8c4, party) + geom_known_state(pop8c4, party) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, make.names(paste0("ger_8c2_",party,"_de.jpg"))), width = 6, height = 3)
  }

  houses <- levels(pd$poll_info$.house)
  for(j in seq_along(houses)){
    # j <- 1
    house <- houses[j]
    expect_silent(
      plt <- plot_parameters_areas(pop8c4,
                                   paste0("beta_sigma[",1:(length(ed$date)+1),",",j,"]"),
                                   paste0("beta_sigma[",c(lubridate::year(ed$date), "2022"),",",house,"]"),
                                   title = paste0("Design effect of ", house))
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, make.names(paste0("ger_",house,"_design_effect.jpg"))), width = 6, height = 4)
  }

  expect_silent(
    plt <- plot_parameters_areas(pop8c4, paste0("sigma_x[", seq_along(pop8c4$y), "]"), pop8c4$y, title = "Posteriord distribution for sigma_x")
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("ger_sigma_x_8c2.jpg")), width = 4, height = 3)

})
