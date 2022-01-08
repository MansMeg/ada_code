context("model8a")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)

test_that("Test model 8a3 and 8a4: Industry bias with different geometry", {
  # The best industry bias models are 8a3 (one sigma_kappa for all parties),
  # and 8a4 (one sigma_kappa per party)
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  print_plt <- FALSE
  tmp_folder <- "tmp_figs/toy_industry"

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
    sd1 <- stan_polls_data(x = spd,
                           time_scale = time_scale,
                           y_name = c("x3", "x4"),
                           model = "model8a",
                           known_state = known_state,
                           hyper_parameters = list(sigma_kappa_hyper = 0.001))
  )
  sd <- sd1

  suppressWarnings(
    sd2 <- stan_polls_data(x = spd,
                           time_scale = time_scale,
                           y_name = c("x3", "x4"),
                           model = "model8a3",
                           known_state = known_state,
                           hyper_parameters = list(sigma_kappa_hyper = 0.001))
  )
  test_sd1 <- sd1$stan_data; test_sd1$g <- NULL
  test_sd2 <- sd2$stan_data; test_sd2$g <- NULL

  # Assert that g is computed in different ways
  expect_equal(test_sd1, test_sd2)
  expect_equal(sd1$stan_data$g * 7 / 365, sd2$stan_data$g, tolerance = 0.0001)

  suppressWarnings(
    sd4 <- stan_polls_data(x = spd,
                           time_scale = time_scale,
                           y_name = c("x3", "x4"),
                           model = "model8a4",
                           known_state = known_state,
                           hyper_parameters = list(sigma_kappa_hyper = 0.001))
  )

  suppressWarnings(
    sd3 <- stan_polls_data(x = spd,
                           time_scale = time_scale,
                           y_name = c("x3", "x4"),
                           model = "model8a3",
                           known_state = known_state)
  )

  expect_equal(sd2, sd4)
  expect_equal(sd3$stan_data$sigma_kappa_hyper, 0.005)

  # Simulate new with kappa = 0.02 per year
  kappa_x3 <- 0.05
  kappa_x4 <- -0.05
  spd$y[,"x3"] <-  spd$y[,"x3"] + sd3$stan_data$g * kappa_x3
  spd$y[,"x4"] <-  spd$y[,"x4"] + sd3$stan_data$g * kappa_x4
  # plot(spd, "x3")
  # plot(spd, "x4")

  expect_silent(pop8a3_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8a3 <- poll_of_polls(y = parties,
                                               model = "model8a3",
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

  # Chain 4:  Elapsed Time: 12.694 seconds (Warm-up)
  # Chain 4:                1.35825 seconds (Sampling)
  # Chain 4:                14.0522 seconds (Total)
  # 2: There were 2 divergent transitions after warmup.


  expect_silent(pop8a4_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8a4 <- poll_of_polls(y = parties,
                                                model = "model8a4",
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
  # Chain 4:  Elapsed Time: 11.4869 seconds (Warm-up)
  # Chain 4:                1.24378 seconds (Sampling)
  # Chain 4:                12.7307 seconds (Total)
  # 2: There were 61 divergent transitions after warmup.

  # Check that it returns a pop object.
  expect_s3_class(pop8a3, "poll_of_polls")
  expect_s3_class(pop8a4, "poll_of_polls")

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "x3"
    expect_silent(
      plt <- plot(pop8a3, party) + geom_known_state(pop8a3, party) + geom_pop_line(pop8a3, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8a3_",party,".jpg")), width = 6, height = 3)
    expect_silent(
      plt <- plot(pop8a4, party) + geom_known_state(pop8a4, party) + geom_pop_line(pop8a3, txdf[[party]]) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8a4_",party,".jpg")), width = 6, height = 3)
  }


  expect_silent(
    plt <- plot_parameters_areas(pop8a4, c("sigma_kappa[1]", "sigma_kappa[2]"), c("sigma_kappa[x3]", "sigma_kappa[x4]"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8a4_sigma_kappa.jpg")), width = 4, height = 3)
  expect_silent(
    plt <- plot_parameters_areas(pop8a4, c("kappa[1,1]", "kappa[2,1]", "kappa[3,1]") , c("kappa[1,x3]", "kappa[2,x3]", "kappa[3,x3]"), title = paste0("Kappa (true value = ",kappa_x3,")"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8a4_kappa_x3.jpg")), width = 4, height = 3)
  expect_silent(
    plt <- plot_parameters_areas(pop8a4, c("kappa[1,2]", "kappa[2,2]", "kappa[3,2]") , c("kappa[1,x4]", "kappa[2,x4]", "kappa[3,x4]"), title = paste0("Kappa (true value = ",kappa_x4,")"))
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("toy_8a4_kappa_x4.jpg")), width = 4, height = 3)

  # Test that the model work for missing values
  ys <- y(spd)
  ys[c(6, 19, 72),2] <- NA
  ys[c(14, 16, 87),3] <- NA
  y(spd) <- ys
  ys <- y(spd)
  ys[124:150, 2] <- NA
  ys[1:31, 3] <- NA
  y(spd) <- ys

  expect_silent(pop8a_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8a3_with_missings <- poll_of_polls(y = parties,
                                                              model = "model8a3",
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

  # Chain 4:  Elapsed Time: 11.4869 seconds (Warm-up)
  # Chain 4:                1.24378 seconds (Sampling)
  # Chain 4:                12.7307 seconds (Total)
  # 2: There were 61 divergent transitions after warmup

})



test_that("test model 8a3 and 8a4: Industry bias in Sweden", {
  skip_if_not(adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  start_date <- as.Date("2010-01-01")
  end_date <- as.Date("2021-06-30")
  parties <- c("S", "M", "SD", "MP", "L", "KD", "V", "C")
  data("swedish_elections")
  data("swedish_polls_curated")

  # Store testresults for checking
  print_plt <- TRUE
  tmp_folder <- "tmp_figs/swe_industry"

  pd <- polls_data(y = swedish_polls_curated[, parties],
                   house = swedish_polls_curated$Company,
                   publish_date = swedish_polls_curated$PublDate,
                   start_date = swedish_polls_curated$collectPeriodFrom,
                   end_date = swedish_polls_curated$collectPeriodTo,
                   n = swedish_polls_curated$n)
  pd <- subset_dates(pd, from = start_date, to = end_date)
  pd <- pd[complete_poll_info(pd)]

  ed <- swedish_elections
  ed$date <- ed$PublDate

  time_scale <- "week"
  set.seed(4711)

  expect_silent(
    sd <- stan_polls_data(x = pd,
                          time_scale = time_scale,
                          y_name = parties,
                          model = "model8a3",
                          known_state = ed)
  )

  expect_silent(pop8a3_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8a3 <- poll_of_polls(y = parties,
                                                model = "model8a3",
                                                polls_data = pd,
                                                time_scale = time_scale,
                                                known_state = ed,
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )
  # Chain 4:  Elapsed Time: 763.564 seconds (Warm-up)
  # Chain 4:                42.729 seconds (Sampling)
  # Chain 4:                806.293 seconds (Total)

  td <- do.call(rbind, get_sampler_params(pop8a3, inc_warmup = FALSE))[, "treedepth__"]
  table(td)
  # td
  # 7   8
  # 86 914

  expect_silent(pop8a4_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8a4 <- poll_of_polls(y = parties,
                                                model = "model8a4",
                                                polls_data = pd,
                                                time_scale = time_scale,
                                                known_state = ed,
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )
  # Chain 4:  Elapsed Time: 3488.41 seconds (Warm-up)
  # Chain 4:                83.5959 seconds (Sampling)
  # Chain 4:                3572.01 seconds (Total)

  # write_ada_json(x = pop8a4)
  # write_latent_mean_csv(pop8a4, "tmp_output/latent_mean_model8a4.csv")

  td <- do.call(rbind, get_sampler_params(pop8a4, inc_warmup = FALSE))[, "treedepth__"]
  table(td)

  # Check that it returs a pop object.
  expect_s3_class(pop8a3, "poll_of_polls")
  expect_s3_class(pop8a4, "poll_of_polls")


  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "L"
    expect_silent(
      plt <- plot(pop8a4, party) + geom_known_state(pop8a4, party) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("swe_",party,"_industry_bias.jpg")), width = 8, height = 3)
  }

  expect_silent(
    plt <- plot_parameters_areas(pop8a4, paste0("sigma_x[", 1:8, "]"), pop8a4$y, title = "Posteriord distribution for sigma_x")
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("swe_sigma_x_industry.jpg")), width = 4, height = 3)

  for(i in seq_along(parties)){
    # i <- 1
    expect_silent(
      plt <- plot_parameters_areas(pop8a4, paste0("kappa[", 1:4, ",",i,"]"), title = paste0("Posteriord distribution for kappas (", parties[i], ")"))
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("kappa_", parties[i],".jpg")), width = 4, height = 3)
  }

  expect_silent(
    plt <- plot_parameters_areas(pop8a4, paste0("sigma_kappa[", 1:8, "]"), pop8a4$y, title = "Posteriord distribution for sigma_kappa")
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("swe_sigma_kappa.jpg")), width = 6, height = 4)

})


test_that("test model 8a3 and 8a4: Industry bias in Germany", {
  skip_if_not(adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  start_date <- as.Date("2008-01-01")
  end_date <- as.Date("2020-01-01")
  parties <- c("CDU/CSU", "SPD", "GRUNE", "FDP", "LINKE", "AfD")
  data("german_elections")
  data("german_polls_curated")

  # Store testresults for checking
  print_plt <- TRUE
  tmp_folder <- "tmp_figs/ger_industry"

  pd <- polls_data(y = german_polls_curated[, parties],
                   house = german_polls_curated$house,
                   publish_date = german_polls_curated$PublDate,
                   start_date = german_polls_curated$collectPeriodFrom,
                   end_date = german_polls_curated$collectPeriodTo,
                   n = german_polls_curated$n)
  pd <- subset_dates(pd, from = start_date, to = end_date)
  pd <- pd[complete_poll_info(pd)]

  ed <- german_elections
  ed$date <- ed$PublDate

  time_scale <- "week"
  set.seed(4711)

  expect_silent(
    sd <- stan_polls_data(x = pd,
                          time_scale = time_scale,
                          y_name = parties,
                          model = "model8a3",
                          known_state = ed)
  )

  expect_silent(pop8a3_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8a3 <- poll_of_polls(y = parties,
                                                model = "model8a3",
                                                polls_data = pd,
                                                time_scale = time_scale,
                                                known_state = ed,
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )
  # Chain 4:  Elapsed Time: 763.564 seconds (Warm-up)
  # Chain 4:                42.729 seconds (Sampling)
  # Chain 4:                806.293 seconds (Total)

  td <- do.call(rbind, get_sampler_params(pop8a3, inc_warmup = FALSE))[, "treedepth__"]
  table(td)
  # td
  # 7   8
  # 86 914

  expect_silent(pop8a4_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8a4 <- poll_of_polls(y = parties,
                                                model = "model8a4",
                                                polls_data = pd,
                                                time_scale = time_scale,
                                                known_state = ed,
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )
  # Chain 4:  Elapsed Time: 3488.41 seconds (Warm-up)
  # Chain 4:                83.5959 seconds (Sampling)
  # Chain 4:                3572.01 seconds (Total)

  td <- do.call(rbind, get_sampler_params(pop8a4, inc_warmup = FALSE))[, "treedepth__"]
  table(td)

  # Check that it returs a pop object.
  expect_s3_class(pop8a3, "poll_of_polls")
  expect_s3_class(pop8a4, "poll_of_polls")


  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "L"
    expect_silent(
      plt <- plot(pop8a4, party) + geom_known_state(pop8a4, party) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, make.names(paste0("ger_",party,"_industry_bias.jpg"))), width = 8, height = 3)
  }

  expect_silent(
    plt <- plot_parameters_areas(pop8a4, paste0("sigma_x[", seq_along(pop8a4$y), "]"), pop8a4$y, title = "Posteriord distribution for sigma_x")
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("ger_sigma_x_industry.jpg")), width = 4, height = 3)

  for(i in seq_along(parties)){
    # i <- 1
    expect_silent(
      plt <- plot_parameters_areas(pop8a4,
                                   paste0("kappa[", 1:(nrow(pop8a4$known_state)-1), ",",i,"]"),
                                   paste0("kappa[", lubridate::year(pop8a4$known_state$date)[-1], "]"),
                                   title = paste0("Posteriord distribution for kappas (", parties[i], ")"))
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, make.names(paste0("kappa_", parties[i],".jpg"))), width = 4, height = 3)
  }


  expect_silent(
    plt <- plot_parameters_areas(pop8a4, paste0("sigma_kappa[", seq_along(pop8a4$y), "]"), pop8a4$y, title = "Posteriord distribution for sigma_kappa")
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("swe_sigma_kappa.jpg")), width = 6, height = 4)


})




test_that("Test model 8a: Industry bias", {
  skip("Model 8a has problems with the geometry due to g being large (say up to 200)")
  # Hence we improved this with model with models 8a3 and 8a4
})


test_that("Compare model 8a, 8a3 and 8a4 on test data", {
  skip("Skip model 8a - we run models 8a3 and 8a4 instead.")
  # This code can be run if we want to compare,
  # but is not relevant for testing purposes

  skip_if_not(test_stan_full_on_local() | on_github_actions_test_branch())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

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

  # Simulate new with kappa = 0.02 per year
  kappa_x3 <- 0.02
  kappa_x4 <- -0.02
  spd$y[,"x3"] <-  spd$y[,"x3"] + sd3$stan_data$g * kappa_x3
  spd$y[,"x4"] <-  spd$y[,"x4"] + sd3$stan_data$g * kappa_x4
  # plot(spd, "x3")
  # plot(spd, "x4")

  expect_silent(pop8a_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8a <- poll_of_polls(y = c("x3", "x4"),
                                               model = "model8a",
                                               polls_data = spd,
                                               time_scale = time_scale,
                                               known_state = known_state,
                                               hyper_parameters = list(sigma_kappa_hyper = 0.001),
                                               warmup = 2000,
                                               iter = 2500,
                                               chains = 2)
                      )
                    )
                  )
  )

  # Chain 1:  Elapsed Time: 81.6146 seconds (Warm-up)
  # Chain 1:                14.9879 seconds (Sampling)
  # Chain 1:                96.6025 seconds (Total)
  # Chain 2:  Elapsed Time: 73.9696 seconds (Warm-up)
  # Chain 2:                14.7956 seconds (Sampling)
  # Chain 2:                88.7652 seconds (Total)

  td <- do.call(rbind, get_sampler_params(pop8a, inc_warmup = FALSE))[, "treedepth__"]
  table(td)
  #  td
  # 7   8   9  10
  # 214 748  35   3

  expect_silent(pop8a3_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8a3 <- poll_of_polls(y = c("x3", "x4"),
                                                model = "model8a3",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(sigma_kappa_hyper = 0.02),
                                                warmup = 2000,
                                                iter = 2500,
                                                chains = 2)
                      )
                    )
                  )
  )

  # Chain 1:  Elapsed Time: 11.5592 seconds (Warm-up)
  # Chain 1:                2.58806 seconds (Sampling)
  # Chain 1:                14.1472 seconds (Total)
  # Chain 2:  Elapsed Time: 11.2682 seconds (Warm-up)
  # Chain 2:                2.76188 seconds (Sampling)
  # Chain 2:                14.0301 seconds (Total)
  # 2: There were 25 divergent transitions after warmup

  td <- do.call(rbind, get_sampler_params(pop8a3, inc_warmup = FALSE))[, "treedepth__"]
  table(td)
  #  td
  # 4   5   6   7   8
  # 23 808 133  35   1

  expect_silent(pop8a4_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8a4 <- poll_of_polls(y = c("x3", "x4"),
                                                model = "model8a4",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = list(sigma_kappa_hyper = 0.02),
                                                warmup = 2000,
                                                iter = 2500,
                                                chains = 2)
                      )
                    )
                  )
  )
  # Chain 1:  Elapsed Time: 11.098 seconds (Warm-up)
  # Chain 1:                2.78732 seconds (Sampling)
  # Chain 1:                13.8853 seconds (Total)
  # Chain 2:  Elapsed Time: 12.2041 seconds (Warm-up)
  # Chain 2:                2.61201 seconds (Sampling)
  # Chain 2:                14.8161 seconds (Total)
  # 2: There were 23 divergent transitions after warmup.

  td <- do.call(rbind, get_sampler_params(pop8a4, inc_warmup = FALSE))[, "treedepth__"]
  table(td)
  #  td
  # 1   2   3   4   5   6   7   8
  # 1   1   5  23 799 141  28   2


})

