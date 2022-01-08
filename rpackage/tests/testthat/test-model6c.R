context("model6c")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)

test_that("test model 6c - ts:week, x_data = 1, with and without some known states", {
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

  expect_error(pop6c_out <-
    capture.output(
      suppressWarnings(
        suppressMessages(
          pop6c <- poll_of_polls(y = c("x3", "x4"),
                                 model = "model6c",
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

  expect_silent(pop6c_out <-
                 capture.output(
                   suppressWarnings(
                     suppressMessages(
                       pop6c <- poll_of_polls(y = c("x3", "x4"),
                                              model = "model6c",
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
  expect_s3_class(pop6c, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  expect_silent(ls <- latent_state(pop6c))

  post_ls_sd <- apply(ls$latent_state[,,"x3"], sd, MARGIN = 2)
  expect_true(all(post_ls_sd[45:71] > 0.000001))
  expect_true(all(post_ls_sd[true_idx] < 0.000001))
  post_ls_mn <- apply(ls$latent_state[,,"x3"], mean, MARGIN = 2)
  expect_true(all(post_ls_mn[15:100] > 0.2))

  # Check that the pop object can be plotted
  expect_silent(
    plot(pop6c, "x3", from = "2010-04-01") + geom_pop_line(pop6c, txdf$x3)
  )

  post_ls_sd <- apply(ls$latent_state[,,2], sd, MARGIN = 2)
  expect_true(all(post_ls_sd[45:71] > 0.000001))
  expect_true(all(post_ls_sd[true_idx] < 0.000001))
  post_ls_mn <- apply(ls$latent_state[,,2], mean, MARGIN = 2)
  expect_true(all(post_ls_mn[1] > 0.2))
  expect_true(all(post_ls_mn[76:100] < 0.0001))


  expect_silent(
    plot_poll_of_polls(x = pop6c, y = "x4", from = "2011-01-01", collection_period = TRUE)
  )
  expect_silent(
    plot_poll_of_polls(pop6c, "x4", from = "2010-01-01")
  )

  skip("Check that there is no duplictes in the y variable")

})


