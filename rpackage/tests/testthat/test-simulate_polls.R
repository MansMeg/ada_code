context("simulate_polls")

test_that("polls_data constructor works", {
  data("swedish_polls_curated")
  swedish_polls <- swedish_polls_curated
  expect_silent(pd <- polls_data(y = swedish_polls[,3:11],
                                 house = swedish_polls$Company,
                                 publish_date = swedish_polls$PublDate,
                                 start_date = swedish_polls$collectPeriodFrom,
                                 end_date = swedish_polls$collectPeriodTo,
                                 n = swedish_polls$n))
  expect_silent(pd <- subset_publish_dates(x = pd, from = "2006-01-01", to = "2015-12-31"))
  expect_silent(pd <- subset(pd, !is.na(pd$poll_info$.start_date) & !is.na(pd$poll_info$.end_date) & !is.na(pd$poll_info$.n)))

  data(x_test)
  expect_silent(spd <- simulate_polls(x = x_test[[4]], pd, npolls = 100, time_scale = "week", start_date = "2010-01-04"))
})

test_that("reweight and resample works", {
  data("swedish_polls_curated")
  swedish_polls <- swedish_polls_curated
  expect_silent(pd <- polls_data(y = swedish_polls[,3:11],
                                 house = swedish_polls$Company,
                                 publish_date = swedish_polls$PublDate,
                                 start_date = swedish_polls$collectPeriodFrom,
                                 end_date = swedish_polls$collectPeriodTo,
                                 n = swedish_polls$n))
  expect_silent(pd <- subset_publish_dates(x = pd, from = "2006-01-01", to = "2010-12-31"))
  expect_silent(pd <- subset(pd, !is.na(pd$poll_info$.start_date) & !is.na(pd$poll_info$.end_date) & !is.na(pd$poll_info$.n)))

  data(x_test)
  set.seed(4711)
  expect_silent(spd1 <- simulate_polls(x = x_test[[1]], pd, npolls = 100, time_scale = "week", start_date = "2010-01-04"))
  expect_silent(ptw1 <- polls_time_weights(spd1))

  expect_silent(spd2 <- adapop:::reweight_and_resample(spd1, x_test[[1]], time_scale = "week"))
  expect_identical(spd1,spd2)

  expect_silent(spd3 <- adapop:::reweight_and_resample(x = spd1, true_ls = x_test[[1]], time_scale = "week", weight = "keep"))
  expect_failure(expect_identical(spd1, spd3))
  expect_identical(ptw1, polls_time_weights(spd3))

  expect_silent(spd4 <- adapop:::reweight_and_resample(spd1, x_test[[1]], time_scale = "week", weight = "skew"))
  expect_failure(expect_identical(spd1, spd4))
  expect_silent(ptw4 <- polls_time_weights(spd4))
  expect_failure(expect_identical(ptw1, polls_time_weights(spd4)))

  ptw4_9 <- ptw4[ptw4$.poll_id == "9",]
  expect_equal(ptw4_9$weight[1], ptw4_9$weight[2])
  expect_equal(ptw4_9$weight[length(ptw4_9$weight)], ptw4_9$weight[2])

  expect_silent(spd5 <- adapop:::reweight_and_resample(spd1, x_test[[1]], time_scale = "week", weight = "vskew"))
  expect_failure(expect_identical(spd1, spd5))
  expect_silent(ptw5 <- polls_time_weights(spd5))
  expect_failure(expect_identical(ptw1, polls_time_weights(spd5)))


  ptw5_9 <- ptw5[ptw5$.poll_id == "9",]
  expect_equal(ptw5_9$weight[1], ptw5_9$weight[2])
  expect_equal(ptw5_9$weight[length(ptw5_9$weight)], ptw5_9$weight[1])
  expect_equal(ptw5_9$weight[1], ptw4_9$weight[1])

  skip("Add test for poll_of_polls object, latent state, elpd, rmse etc")
})


