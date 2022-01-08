context("stan_polls_data")

test_that("stan_polls_data works", {
  data("swedish_polls_curated")
  swedish_polls <- swedish_polls_curated
  expect_silent(pd <- polls_data(y = swedish_polls[,3:11],
                                 house = swedish_polls$Company,
                                 publish_date = swedish_polls$PublDate,
                                 start_date = swedish_polls$collectPeriodFrom,
                                 end_date = swedish_polls$collectPeriodTo,
                                 n = swedish_polls$n))
  expect_silent(pd <- subset_publish_dates(x = pd, from = "2006-01-01"))
  expect_silent(pd <- subset(pd, !is.na(pd$poll_info$.start_date) & !is.na(pd$poll_info$.end_date) & !is.na(pd$poll_info$.n)))
  time_range(pd) <- time_range_polls(pd)
  pd2 <- pd[1:30]
  time_range(pd2) <- time_range_polls(pd2)
  expect_silent(sd <- stan_polls_data_model2(x = pd2, time_scale = "week", y_name = "S"))
  expect_silent(sd2 <- stan_polls_data(x = pd2, time_scale = "week", y = "S", model = "model2"))
  sd[[1]]$time_scale_length <- sd2[[1]]$time_scale_length
  expect_identical(sd,sd2)

  # plt <- plot(pd2, y = "S") + geom_stan_polls_data(x = sd, size = 0.2, alpha = 0.5)

  expect_silent(tws1 <- polls_time_weights(x = sd))
  expect_silent(tws2 <- polls_time_weights(x = pd2))
  expect_silent(tws2 <- summarize_polls_time_weights(tws2, sd$time_line))
  expect_identical(tws1, tws2)

  # Check that i and poll_id give  same result
  tws1$i <- sd$stan_data$tw_i
  tws1$S <- sd$stan_data$y[tws1$i]
  tws1 <- dplyr::left_join(tws1, pd2$y[,c(".poll_id", "S")], by =".poll_id")
  expect_identical(tws1$S.x, tws1$S.y)


  expect_silent(sd1 <- stan_polls_data_model3(x = pd2, time_scale = "week", y = "S"))
  expect_silent(sd2 <- stan_polls_data(x = pd2, time_scale = "week", y = "S", model = "model3"))
  expect_identical(sd1, sd2)

  expect_silent(sd1 <- stan_polls_data_model3(x = pd2, time_scale = "day", y = "S"))
  expect_silent(sd2 <- stan_polls_data(x = pd2, time_scale = "day", y = "S", model = "model3"))
  expect_identical(sd1, sd2)

})



test_that("stan_polls_data works with overlapping error", {
  data("swedish_polls_curated")
  swedish_polls <- swedish_polls_curated
  swe_polls <- polls_data(y = swedish_polls[,3:11]/100,
                          house = swedish_polls$Company,
                          publish_date = swedish_polls$PublDate,
                          start_date = swedish_polls$collectPeriodFrom,
                          end_date = swedish_polls$collectPeriodTo,
                          n = swedish_polls$n)
  swe_polls <- subset_publish_dates(swe_polls, "2010-01-01")
  swe_polls <- swe_polls[complete_poll_info(swe_polls)]

  expect_error(sd <- stan_polls_data(x = swe_polls, time_scale = "week", y = "S", model = "model5"), regexp = "2009-12-30")
  expect_error(sd <- stan_polls_data(x = swe_polls, time_scale = "week", y = "S", model = "model5"), regexp = "The following date")

})



test_that("stan_data model6b", {
  data("swedish_polls_curated")
  data("swedish_elections")
  swedish_polls <- swedish_polls_curated
  swe_polls <- polls_data(y = swedish_polls[,3:11],
                          house = swedish_polls$Company,
                          publish_date = swedish_polls$PublDate,
                          start_date = swedish_polls$collectPeriodFrom,
                          end_date = swedish_polls$collectPeriodTo,
                          n = swedish_polls$n)
  swe_polls <- subset_dates(swe_polls, "2010-01-01")
  swe_polls <- swe_polls[complete_poll_info(swe_polls)]
  swedish_elections$date <- swedish_elections$PublDate

  expect_silent(sd1 <- stan_polls_data(x = swe_polls, time_scale = "week", y = "S", model = "model6b"))
  expect_error(sd2 <- stan_polls_data(x = swe_polls, time_scale = "week", y = "S", model = "model6b", known_state = swedish_elections))
  expect_silent(sd2 <- stan_polls_data(x = swe_polls, time_scale = "week", y = "S", model = "model6b", known_state = swedish_elections[swedish_elections$date > as.Date("2010-01-01"),]))

})


test_that("stan_data model8a", {
  data("swedish_polls_curated")
  data("swedish_elections")
  swedish_polls <- swedish_polls_curated
  swe_polls <- polls_data(y = swedish_polls[,3:11],
                          house = swedish_polls$Company,
                          publish_date = swedish_polls$PublDate,
                          start_date = swedish_polls$collectPeriodFrom,
                          end_date = swedish_polls$collectPeriodTo,
                          n = swedish_polls$n)
  swe_polls <- subset_dates(swe_polls, from = "2010-01-01", to = "2019-12-31" )
  swe_polls <- swe_polls[complete_poll_info(swe_polls)]
  swedish_elections$date <- swedish_elections$PublDate

  expect_error(sd1 <- stan_polls_data(x = swe_polls, time_scale = "week", y_name = "S", model = "model8a"), regexp = "known_state")
  expect_silent(sd2 <- stan_polls_data(x = swe_polls, time_scale = "week", y_name = "S", model = "model8a", known_state = swedish_elections))
  expect_identical(sd2$stan_data$next_known_state[1], sd2$stan_data$T_known + 1)
  expect_identical(sd2$stan_data$next_known_state[sd2$stan_data$N], 1)

  pid <- swe_polls$poll_info
  pid <- dplyr::left_join(pid, sd2$poll_ids, by = ".poll_id")
  pidx1 <- pid[pid$.house == "SCB" & pid$.publish_date == "2017-06-01", ]$i
  pidx2 <- pid[pid$.house == "Novus" & pid$.publish_date == "2014-12-16", ]$i

  expect_equal(sd2$stan_data$g[sd2$poll_id$i[sd2$poll_ids$.poll_id == pidx1]], 206)
  expect_equal(sd2$stan_data$g[sd2$poll_id$i[sd2$poll_ids$.poll_id == pidx2]], 85)

  expect_warning(sd3 <- stan_polls_data(x = swe_polls, time_scale = "week", y_name = "S", model = "model8a", known_state = swedish_elections[swedish_elections$date > as.Date("2010-01-01"),]))
  expect_equal(sd2$stan_data$g[sd2$poll_id$i[sd2$poll_ids$.poll_id == pidx1]], sd3$stan_data$g[sd2$poll_id$i[sd3$poll_ids$.poll_id == pidx1]])
  expect_equal(sd2$stan_data$g[sd2$poll_id$i[sd2$poll_ids$.poll_id == pidx2]], sd3$stan_data$g[sd3$poll_id$i[sd3$poll_ids$.poll_id == pidx2]])
  expect_lt(sd3$stan_data$g[length(sd3$stan_data$g)], sd2$stan_data$g[length(sd2$stan_data$g)])
})


test_that("stan_data model8b", {
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
  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)

  # Add houses
  spd$poll_info$.house <- as.factor(sample(c("A", "B"), replace = TRUE, size = length(spd$poll_info$.house)))

  # Add house bias
  bias_A_x3 <- 0.01;bias_B_x3 <- -0.02;bias_B_x4 <- 0.03
  spd$y[spd$poll_info$.house == "A", "x3"] <-
    spd$y[spd$poll_info$.house == "A", "x3"] + bias_A_x3
  spd$y[spd$poll_info$.house == "B", "x3"] <-
    spd$y[spd$poll_info$.house == "B", "x3"] + bias_B_x3
  spd$y[spd$poll_info$.house == "B", "x4"] <-
    spd$y[spd$poll_info$.house == "B", "x4"] + bias_B_x4

  expect_warning(
    suppressMessages(
      sd <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8b",
                            known_state = known_state,
                            slow_scales = as.Date(c("2000-01-01", "2010-06-01", "2011-01-01", "2011-06-01")))
    )
  )
  expect_true(sd$stan_data$S == 5)
  expect_true(all(sd$stan_data$s_i != 1))

  expect_warning(
    suppressMessages(
      sd <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8b",
                            known_state = known_state,
                            slow_scales = as.Date(c("2010-06-01", "2011-01-01", "2011-06-01", "2011-12-31")))
    )
  )
  expect_true(sd$stan_data$S == 5)
  expect_true(any(sd$stan_data$s_i == 1))

  s_limits <- as.Date(c("2000-01-01", "2010-06-01", "2011-01-01", "2011-06-01", "2011-12-31"))
  expect_warning(
    suppressMessages(
      sd <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8b",
                            known_state = known_state,
                            slow_scales = s_limits)
    )
  )
  points <- collection_midpoint_dates(spd)
  s <- integer(length(points))
  for(i in seq_along(points)) s[i] <- min(which(points[i] < s_limits))
  expect_equal(sd$stan_data$S, 6)
  expect_equal(sd$stan_data$s_i, s)


  expect_message(
    sd <- stan_polls_data(x = spd,
                          time_scale = time_scale,
                          y_name = c("x3", "x4"),
                          model = "model8b",
                          known_state = known_state,
                          slow_scales = as.Date(c("2010-06-01", "2011-01-01", "2011-06-01")))
  )
  expect_true(sd$stan_data$S == 4)
  expect_true(all(table(sd$stan_data$s_i) > 1))


  expect_message(
    sd <- stan_polls_data(x = spd,
                          time_scale = time_scale,
                          y_name = c("x3", "x4"),
                          model = "model8b",
                          known_state = known_state)
  )
  expect_true(sd$stan_data$S == 1)
  expect_true(all(sd$stan_data$s_i == 1))

})

test_that("stan_data model8c", {
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
  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)

  # Add houses
  spd$poll_info$.house <- as.factor(sample(c("A", "B"), replace = TRUE, size = length(spd$poll_info$.house)))

  # Add house bias
  bias_A_x3 <- 0.01;bias_B_x3 <- -0.02;bias_B_x4 <- 0.03
  spd$y[spd$poll_info$.house == "A", "x3"] <-
    spd$y[spd$poll_info$.house == "A", "x3"] + bias_A_x3
  spd$y[spd$poll_info$.house == "B", "x3"] <-
    spd$y[spd$poll_info$.house == "B", "x3"] + bias_B_x3
  spd$y[spd$poll_info$.house == "B", "x4"] <-
    spd$y[spd$poll_info$.house == "B", "x4"] + bias_B_x4

  expect_warning(
    suppressMessages(
      sd <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8c",
                            known_state = known_state,
                            slow_scales = as.Date(c("2000-01-01", "2010-06-01", "2011-01-01", "2011-06-01")))
    )
  )
  expect_true(sd$stan_data$S == 5)
  expect_true(all(sd$stan_data$s_i != 1))

  expect_warning(
    suppressMessages(
      sd <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8c",
                            known_state = known_state,
                            slow_scales = as.Date(c("2010-06-01", "2011-01-01", "2011-06-01", "2011-12-31")))
    )
  )
  expect_true(sd$stan_data$S == 5)
  expect_true(any(sd$stan_data$s_i == 1))

  s_limits <- as.Date(c("2000-01-01", "2010-06-01", "2011-01-01", "2011-06-01", "2011-12-31"))
  expect_warning(
    suppressMessages(
      sd <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8c",
                            known_state = known_state,
                            slow_scales = s_limits)
    )
  )
  points <- collection_midpoint_dates(spd)
  s <- integer(length(points))
  for(i in seq_along(points)) s[i] <- min(which(points[i] < s_limits))
  expect_equal(sd$stan_data$S, 6)
  expect_equal(sd$stan_data$s_i, s)


  expect_message(
    sd <- stan_polls_data(x = spd,
                          time_scale = time_scale,
                          y_name = c("x3", "x4"),
                          model = "model8c",
                          known_state = known_state,
                          slow_scales = as.Date(c("2010-06-01", "2011-01-01", "2011-06-01")))
  )
  expect_true(sd$stan_data$S == 4)
  expect_true(all(table(sd$stan_data$s_i) > 1))
  expect_s3_class(sd, "model8c")

  expect_message(
    sd2 <- stan_polls_data(x = spd,
                          time_scale = time_scale,
                          y_name = c("x3", "x4"),
                          model = "model8c2",
                          known_state = known_state,
                          slow_scales = as.Date(c("2010-06-01", "2011-01-01", "2011-06-01")))
  )
  expect_s3_class(sd2, "model8c")


  expect_message(
    sd <- stan_polls_data(x = spd,
                          time_scale = time_scale,
                          y_name = c("x3", "x4"),
                          model = "model8c",
                          known_state = known_state)
  )
  expect_true(sd$stan_data$S == 1)
  expect_true(all(sd$stan_data$s_i == 1))

})



test_that("stan_data is identical for model 8a and model 8d", {

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

  expect_silent(
    suppressWarnings(
      sd8a3 <- stan_polls_data(x = spd,
                              time_scale = time_scale,
                              y_name = c("x3", "x4"),
                              model = "model8a3",
                              known_state = known_state,
                              hyper_parameters = list(sigma_kappa_hyper = 0.001))
    )
  )
  expect_message(
    suppressWarnings(
      sd8d <- stan_polls_data(x = spd,
                              time_scale = time_scale,
                              y_name = c("x3", "x4"),
                              model = "model8d",
                              known_state = known_state,
                              hyper_parameters = list(sigma_kappa_hyper = 0.001))
    )
  )
  sd8d_full <- sd8d
  sd8d$stan_data$use_industry_bias <- NULL
  sd8d$stan_data$use_house_bias <- NULL
  sd8d$stan_data$use_design_effects <- NULL
  sd8d$stan_data$beta_mu_1_sigma_hyper <- NULL
  sd8d$stan_data$sigma_beta_mu_sigma_hyper <- NULL
  sd8d$stan_data$beta_sigma_sigma_hyper <- NULL
  sd8d$stan_data$sigma_beta_sigma_sigma_hyper <- NULL
  sd8d$stan_data$H <- NULL
  sd8d$stan_data$h_i <- NULL
  sd8d$stan_data$S <- NULL
  sd8d$stan_data$s_i <- NULL

  expect_identical(sd8a3$stan_data, sd8d$stan_data)

})


test_that("stan_data is identical for model 8b and model 8d", {

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
      sd8b  <- stan_polls_data(x = spd,
                               time_scale = time_scale,
                               y_name = c("x3", "x4"),
                               model = "model8b",
                               known_state = known_state)
    )
  )
  expect_message(
    suppressWarnings(
      sd8d <- stan_polls_data(x = spd,
                              time_scale = time_scale,
                              y_name = c("x3", "x4"),
                              model = "model8d",
                              known_state = known_state)
    )
  )
  sd8d_full <- sd8d
  sd8d$stan_data$use_industry_bias <- NULL
  sd8d$stan_data$use_house_bias <- NULL
  sd8d$stan_data$use_design_effects <- NULL
  sd8d$stan_data$next_known_state_index <- NULL
  sd8d$stan_data$g <- NULL
  sd8d$stan_data$sigma_kappa_hyper <- NULL
  sd8d$stan_data$beta_sigma_sigma_hyper <- NULL
  sd8d$stan_data$sigma_beta_sigma_sigma_hyper <- NULL
  # names(sd8b$stan_data);names(sd8d$stan_data)
  expect_identical(sd8b$stan_data, sd8d$stan_data)

})


test_that("stan_data is identical for model 8c and model 8d", {

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
      sd8c  <- stan_polls_data(x = spd,
                               time_scale = time_scale,
                               y_name = c("x3", "x4"),
                               model = "model8c",
                               known_state = known_state)
    )
  )
  expect_message(
    suppressWarnings(
      sd8d <- stan_polls_data(x = spd,
                              time_scale = time_scale,
                              y_name = c("x3", "x4"),
                              model = "model8d",
                              known_state = known_state)
    )
  )
  sd8d_full <- sd8d
  sd8d$stan_data$use_industry_bias <- NULL
  sd8d$stan_data$use_house_bias <- NULL
  sd8d$stan_data$use_design_effects <- NULL
  sd8d$stan_data$next_known_state_index <- NULL
  sd8d$stan_data$g <- NULL
  sd8d$stan_data$sigma_kappa_hyper <- NULL
  sd8d$stan_data$sigma_beta_mu_sigma_hyper <- NULL
  sd8d$stan_data$beta_mu_1_sigma_hyper <- NULL
  # names(sd8c$stan_data);names(sd8d$stan_data)
  expect_identical(sd8c$stan_data, sd8d$stan_data)

})

