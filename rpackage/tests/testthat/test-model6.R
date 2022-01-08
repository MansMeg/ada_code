context("model6")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)

test_that("test model 6 - ts:week, x_data = 1, with and without some known states", {
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
  true_idx <- c(1, 25, 44, 72, 100)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  expect_silent(pop6a_out <-
    capture.output(
      suppressWarnings(
        suppressMessages(
          pop6a <- poll_of_polls(y = c("x3", "x4"),
                                 model = "model6",
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
  expect_silent(ls <- latent_state(pop6a))
  post_ls_sd <- apply(ls$latent_state[,,1], sd, MARGIN = 2)
  expect_true(all(post_ls_sd[-true_idx] > 0.000001))
  expect_true(all(post_ls_sd[true_idx] < 0.000001))

  # Check that it returs a pop object.
  expect_s3_class(pop6a, "poll_of_polls")

  # Check that the pop object can be plotted
  expect_silent(
    plot(pop6a, "x3") + geom_pop_line(pop6a, txdf$x3)
  )
  expect_silent(
    plot(pop6a, "x4") + geom_pop_line(pop6a, txdf$x4)
  )
})


