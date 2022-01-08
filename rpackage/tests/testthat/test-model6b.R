context("model6b")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)

test_that("test model 6b - ts:week, x_data = 1, with and without some known states", {
  skip_if_not(test_stan_basic_on_local() | test_stan_full_on_local() | on_github_actions())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  time_scale <- "week"
  set.seed(4711)

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 150,
                        time_scale = time_scale,
                        start_date = "2010-01-01")
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])
  latent_time_ranges <- list("x3" = list(from = "2010-04-01"),
                             "x4" = list(to = "2011-06-01"))

  ys <- y(spd)
  ys[c(6, 19, 72),2] <- NA
  ys[c(14, 16, 87),3] <- NA
  y(spd) <- ys

  expect_error(pop6a_out <-
    capture.output(
      suppressWarnings(
        suppressMessages(
          pop6a <- poll_of_polls(y = c("x3", "x4"),
                                 model = "model6b",
                                 polls_data = spd,
                                 time_scale = time_scale,
                                 known_state = known_state,
                                 latent_time_ranges = latent_time_ranges,
                                 warmup = 1000,
                                 iter = 1500,
                                 chains = 2)
          )
        )
      )
    )

  ys <- y(spd)
  ys[124:150, 2] <- NA
  ys[1:31, 3] <- NA
  y(spd) <- ys

  expect_silent(pop6a_out <-
                 capture.output(
                   suppressWarnings(
                     suppressMessages(
                       pop6a <- poll_of_polls(y = c("x3", "x4"),
                                              model = "model6b",
                                              polls_data = spd,
                                              time_scale = time_scale,
                                              known_state = known_state,
                                              latent_time_ranges = latent_time_ranges,
                                              warmup = 1000,
                                              iter = 1500,
                                              chains = 2)
                     )
                   )
                 )
  )

  # Check that it returs a pop object.
  expect_s3_class(pop6a, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  expect_silent(ls <- latent_state(pop6a))

  post_ls_sd <- apply(ls$latent_state[,,1], sd, MARGIN = 2)
  expect_true(all(post_ls_sd[45:71] > 0.000001))
  expect_true(all(post_ls_sd[true_idx] < 0.000001))
  post_ls_mn <- apply(ls$latent_state[,,1], mean, MARGIN = 2)
  expect_true(all(post_ls_mn[15:100] > 0.2))

  # Check that the pop object can be plotted
  expect_silent(
    plot(pop6a, "x3", from = "2010-04-01") + geom_pop_line(pop6a, txdf$x3)
  )

  post_ls_sd <- apply(ls$latent_state[,,2], sd, MARGIN = 2)
  expect_true(all(post_ls_sd[45:71] > 0.000001))
  expect_true(all(post_ls_sd[true_idx] < 0.000001))
  post_ls_mn <- apply(ls$latent_state[,,2], mean, MARGIN = 2)
  expect_true(all(post_ls_mn[1] > 0.2))
  ##
  # 
  #should have zero due to deos not exists any mode
  ##
  expect_true(all(post_ls_mn[76:100] < 0.0001))


  expect_silent(
    plot_poll_of_polls(x = pop6a, y = "x4", from = "2011-01-01", collection_period = TRUE)
  )
  expect_silent(
    plot_poll_of_polls(pop6a, "x4", from = "2010-01-01")
  )

  skip("Check that there is no duplictes in the y variable")

})



test_that("test model 6b - ts:week, x_data = 1, with and without some known states", {
  skip_if_not(test_stan_full_on_local() | on_github_actions_test_branch())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  time_scale <- "week"
  set.seed(4711)

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 150,
                        time_scale = time_scale,
                        start_date = "2010-01-01")
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])
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

  expect_silent(pop6b_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop6b <- poll_of_polls(y = c("x3", "x4"),
                                               model = "model6b",
                                               polls_data = spd,
                                               time_scale = time_scale,
                                               known_state = known_state,
                                               latent_time_ranges = latent_time_ranges,
                                               warmup = 1000,
                                               iter = 1500,
                                               chains = 2)
                      )
                    )
                  )
  )

  # Check that it returs a pop object.
  expect_s3_class(pop6b, "poll_of_polls")

  expect_silent(ppd <- poll_predictive_distribution(x = pop6b, poll_ids = c(45, 46)))

})



test_that("test model 6b: Swedish data", {
  skip_if_not(test_stan_full_on_local() | on_github_actions_test_branch())

  start_date <- as.Date("2013-01-01")
  end_date <- as.Date("2019-12-31")
  parties <- c("S", "M", "SD", "MP")
  data("swedish_elections")
  data("swedish_polls_curated")

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
  ed <- ed[ed$PublDate > start_date,]

  time_scale <- "week"
  set.seed(4711)

  expect_silent(
    sd <- stan_polls_data(x = pd,
                          time_scale = time_scale,
                          y_name = parties,
                          model = "model8a",
                          known_state = ed)
  )


  # Running this takes roughly 70s per chain
  expect_error(pop6b_out <-
                 capture.output(
                   suppressWarnings(
                     suppressMessages(
                       pop6b <- poll_of_polls(y = parties,
                                              model = "model6b",
                                              polls_data = pd,
                                              time_scale = time_scale,
                                              known_state = ed,
                                              warmup = 2000,
                                              iter = 2500,
                                              chains = 2)
                     )
                   )
                 )
  )


  # Check that it returs a pop object.
  expect_s3_class(pop6b, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  expect_silent(ls <- latent_state(pop6b))

  # Check that the pop object can be plotted
  expect_silent(
    plt_s <- plot(pop6b, "S")
  )
  # ggsave(plt_s, filename = "test-model6b-S.png", width = 12, height = 4)
  expect_silent(
    plt_m <- plot(pop6b, "M")
  )
  # ggsave(plt_m, filename = "test-model6b-M.png", width = 12, height = 4)
  expect_silent(
    plt_sd <- plot(pop6b, "SD")
  )
  # ggsave(plt_sd, filename = "test-model6b-SD.png", width = 12, height = 4)
  expect_silent(
    plt_mp <- plot(pop6b, "MP")
  )
  # ggsave(plt_mp, filename = "test-model6b-MP.png", width = 12, height = 4)


})


