test_that("subset_publish_dates works", {
  data("swedish_polls_curated")
  swedish_polls <- swedish_polls_curated
  expect_silent(pd <- polls_data(y = swedish_polls[,3:11],
                                 house = swedish_polls$Company,
                                 publish_date = swedish_polls$PublDate,
                                 start_date = swedish_polls$collectPeriodFrom,
                                 end_date = swedish_polls$collectPeriodTo,
                                 n = swedish_polls$n))
  expect_silent(pd <- subset_publish_dates(pd, "2010-01-01", "2015-01-01"))
})


test_that("subset_latent_state_dates works", {

  if(FALSE){
    # Recreate the test data
    time_scale <- "week"
    set.seed(4711)
    spd <- simulate_polls(x = x_test[[1]],
                          pd = pd_test,
                          npolls = 150,
                          time_scale = time_scale,
                          start_date = "2010-01-01")
    pop5a <- poll_of_polls(y = "y",
                           model = "model5",
                           polls_data = spd,
                           time_scale = time_scale,
                           known_state = known_state,
                           warmup = 1000,
                           iter = 1005,
                           chains = 2)
    save(pop5a, file = test_path("test_data/mod5a_test.rda"))
  } else {
    load(file = test_path("test_data/mod5a_test.rda"))
  }

  # Start 2010-01-01 end 2011-11-27
  expect_silent(ls <- latent_state(pop5a))
  expect_silent(ls1 <- subset_latent_state_dates(x = ls, from = "2010-01-01", to = "2011-11-27"))
  expect_equal(ls1, ls)
  expect_warning(ls2 <- subset_latent_state_dates(ls, from = "2009-12-31", to = "2011-11-28"))
  expect_equal(ls2, ls)
  expect_silent(ls3 <- subset_latent_state_dates(ls, to = "2010-12-31"))
  expect_silent(ls4 <- subset_latent_state_dates(ls, from = "2010-01-01", to = "2010-12-31"))
  expect_equal(ls3, ls4)
  expect_silent(ls5 <- subset_latent_state_dates(ls, from = "2010-12-31"))
  expect_silent(ls6 <- subset_latent_state_dates(ls, from = "2010-12-31", to = "2011-11-27"))
  expect_equal(ls5, ls6)
  expect_silent(ls7 <- subset_latent_state_dates(ls, from = "2010-10-01", to = "2010-12-01"))

})

