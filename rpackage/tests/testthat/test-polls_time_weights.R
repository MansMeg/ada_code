context("polls_time_weight")

test_that("polls_time_weights constructor works", {
  data("swedish_polls_curated")
  swedish_polls <- swedish_polls_curated
  expect_silent(pd <- polls_data(y = swedish_polls[,3:11],
                                 house = swedish_polls$Company,
                                 publish_date = swedish_polls$PublDate,
                                 start_date = swedish_polls$collectPeriodFrom,
                                 end_date = swedish_polls$collectPeriodTo,
                                 n = swedish_polls$n))
  expect_silent(pd <- subset_publish_dates(x = pd, from = "2006-01-01", to = "2015-12-31"))

  no_date_na <- !is.na(start_dates(pd)) & !is.na(end_dates(pd))
  no_n_na <- !is.na(n(pd))
  expect_warning(ptw <- polls_time_weights(x = pd), regexp = "missing start or end date")
  expect_equal(nrow(ptw), sum(as.integer(end_dates(pd) - start_dates(pd)), na.rm = TRUE) + length(pd[no_date_na]))

  pd2 <- subset(pd, no_date_na & no_n_na)
  expect_silent(ptw2 <- polls_time_weights(x = pd2))
  expect_equal(nrow(ptw2), sum(as.integer(end_dates(pd2) - start_dates(pd2)), na.rm = TRUE) + length(pd2))

  pd3 <- pd2[1]
  expect_silent(ptw3 <- polls_time_weights(x = pd3))
  expect_equal(nrow(ptw3), sum(as.integer(end_dates(pd3) - start_dates(pd3)), na.rm = TRUE) + length(pd3))

  # Summarize polls
  tl <- time_line(x = time_range_polls(pd3), "week")
  expect_silent(ptw4 <- summarize_polls_time_weights(ptw = ptw3, tl))
  expect_equal(nrow(ptw4), 4)
  expect_equal(sum(ptw4$weight), 1)

  expect_error(ptw5 <- summarize_polls_time_weights(ptw = ptw2, tl))
  tl2 <- time_line(x = time_range_polls(pd2), "week")
  expect_silent(ptw6 <- summarize_polls_time_weights(ptw = ptw2, tl2))
  expect_equal(sum(ptw6$weight), length(pd2))
})


test_that("set time_weights", {
  data("swedish_polls_curated")
  swedish_polls <- swedish_polls_curated
  expect_silent(pd <- polls_data(y = swedish_polls[,3:11],
                                 house = swedish_polls$Company,
                                 publish_date = swedish_polls$PublDate,
                                 start_date = swedish_polls$collectPeriodFrom,
                                 end_date = swedish_polls$collectPeriodTo,
                                 n = swedish_polls$n))
  expect_silent(pd <- pd[complete_poll_info(pd)])

  expect_silent(ptw1 <- ptw2 <- polls_time_weights(x = pd))
  ptw2$weight[1:3] <- c(sum(ptw2$weight[1:3]), 0, 0)

  expect_silent(polls_time_weights(x = pd) <- ptw1)
  expect_equal(polls_time_weights(x = pd), ptw1)
  expect_silent(polls_time_weights(x = pd) <- ptw2)
  expect_failure(expect_equal(polls_time_weights(x = pd), ptw1))
  expect_equal(polls_time_weights(x = pd), ptw2)
  expect_error(polls_time_weights(x = pd) <- ptw1[1:100,])
  expect_error(polls_time_weights(x = pd) <- ptw1[-91,])

  ptw1$date[5] <- as.Date("1900-01-01")
  expect_error(polls_time_weights(x = pd) <- ptw1)
})

test_that("set time_weights", {
  data("swedish_polls_curated")
  swedish_polls <- swedish_polls_curated
  expect_silent(pd <- polls_data(y = swedish_polls[,3:11],
                                 house = swedish_polls$Company,
                                 publish_date = swedish_polls$PublDate,
                                 start_date = swedish_polls$collectPeriodFrom,
                                 end_date = swedish_polls$collectPeriodTo,
                                 n = swedish_polls$n))
  pd <- subset_dates(pd, from = "2010-01-01", to = "2019-12-31" )
  expect_silent(pd <- pd[complete_poll_info(pd)])

  expect_silent(x <- ptw1 <- polls_time_weights(x = pd))

  expect_silent(ptw2 <- polls_time_reweight(ptw1, poll_ids = poll_ids(pd)[1], stats::dnorm, sd = 3, mean = 0))
  expect_silent(ptw3 <- polls_time_reweight(ptw1, poll_ids(pd)[c(1,3)], stats::dnbinom, size = 3, mu = 7))
  expect_silent(ptw4 <- polls_time_reweight(ptw1, poll_ids(pd), stats::dnbinom, size = 3, mu = 7))

  expect_silent(polls_time_weights(pd) <- ptw2)
  expect_silent(polls_time_weights(pd) <- ptw3)
  expect_silent(polls_time_weights(pd) <- ptw4)

})


