context("model5")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)

test_that("Test Model 5 - ts:week, x_data = 1, with and without some known states", {
  skip_if_not(test_stan_basic_on_local() | test_stan_full_on_local() | on_github_actions())

  time_scale <- "week"
  set.seed(4711)
  spd <- simulate_polls(x = x_test[[1]],
                        pd = pd_test,
                        npolls = 150,
                        time_scale = time_scale,
                        start_date = "2010-01-01")
  true_idx <- c(1, 25, 44, 72, 100)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1) ,
                                x = x_test[[1]][true_idx])

  expect_silent(pop5a_out <-
    capture.output(
      suppressWarnings(
        suppressMessages(
          pop5a <- poll_of_polls(y = "x",
                                 model = "model5",
                                 polls_data = spd,
                                 time_scale = time_scale,
                                 known_state = known_state,
                                 warmup = 1000,
                                 iter = 1500,
                                 chains = 2)
          )
        )
      )
    )

  # Test that the x[known] has no sd while the unknows has
  ls <- latent_state(pop5a)
  post_ls_sd <- apply(ls$latent_state[,,1], sd, MARGIN = 2)
  expect_true(all(post_ls_sd[-true_idx] > 0.000001))
  expect_true(all(post_ls_sd[true_idx] < 0.000001))

  # Check that it returs a pop object.
  expect_s3_class(pop5a, "poll_of_polls")

  # Check that the pop object can be plotted
  expect_silent(
    plot(pop5a, "x")
  )

  expect_silent(pop5b_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop5b <- poll_of_polls(y = "x",
                                               model = "model5",
                                               polls_data = spd,
                                               time_scale = time_scale,
                                               warmup = 2000,
                                               iter = 3000,
                                               chains = 3)
                      )
                    )
                  )
                )

})




test_that("test model 5 - ts:days, x_data = 3, with known states", {
  skip_if_not(test_stan_full_on_local() | on_github_actions_test_branch())

  time_scale <- "day"
  set.seed(4711)
  spd <- simulate_polls(x = x_test[[3]],
                        pd = pd_test,
                        npolls = 150,
                        time_scale = time_scale,
                        start_date = "2010-01-01")
  true_idx <- c(1, 25, 44, 72, 100)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::days(true_idx - 1) ,
                                x = x_test[[3]][true_idx])

  expect_silent(pop5c_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop5c <- poll_of_polls(y = "x",
                                               model = "model5",
                                               polls_data = spd,
                                               time_scale = time_scale,
                                               known_state = known_state,
                                               warmup = 2000,
                                               iter = 3000,
                                               chains = 3)
                      )
                    )
                  )
  )


  # Test that the x[known] has no sd while the unknows has
  ls <- latent_state(pop5c)
  post_ls_sd <- apply(ls$latent_state[,,1], sd, MARGIN = 2)
  expect_true(all(post_ls_sd[-true_idx] > 0.000001))
  expect_true(all(post_ls_sd[true_idx] < 0.000001))

  # Check that it returs a pop object.
  expect_s3_class(pop5c, "poll_of_polls")

  # Check that the pop object can be plotted
  expect_silent(
    plot(pop5c, "x")
  )
})


test_that("test model 5 model_range", {
  skip_if_not(test_stan_full_on_local() | on_github_actions_test_branch())

  time_scale <- "day"
  set.seed(4711)
  spd <- simulate_polls(x = x_test[[3]],
                        pd = pd_test,
                        npolls = 150,
                        time_scale = time_scale,
                        start_date = "2010-01-01")
  true_idx <- c(1, 25, 44, 72, 100)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::days(true_idx - 1) ,
                                x = x_test[[3]][true_idx])
  mtr <- time_range(spd)
  spd <- subset_dates(spd, from = "2010-02-01", "2010-03-15")

  expect_silent(pop5c_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop5c <- poll_of_polls(y = "x",
                                               model = "model5",
                                               polls_data = spd,
                                               time_scale = time_scale,
                                               known_state = known_state,
                                               model_time_range = mtr,
                                               warmup = 1000,
                                               iter = 1500,
                                               chains = 2)
                      )
                    )
                  )
  )

  # Check that it returs a pop object.
  expect_s3_class(pop5c, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  ls <- latent_state(pop5c)
  expect_silent(ls2 <- subset_latent_state_dates(ls, from = "2010-01-01", to = "2010-02-01"))

  # Check that the pop object can be plotted
  expect_silent(
    plot_poll_of_polls(x = pop5c, "x", from = "2010-01-01", to = "2010-02-01")
  )
  expect_silent(
    plot_poll_of_polls(x = pop5c, "x", from = "2010-02-04", to = "2010-04-01", collection_period = TRUE)
  )
  expect_silent(
    plot_poll_of_polls(x = pop5c, "x", from = "2010-01-01", to = "2010-02-01", collection_period = TRUE)
  )
})

