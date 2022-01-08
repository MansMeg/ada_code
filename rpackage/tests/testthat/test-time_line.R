context("time_line")

test_that("time_line constructor works", {
  x <- 1:10
  expect_silent(tl1 <- time_line(x, time_scale = "week", start_date = "2010-01-04"))
  checkmate::expect_data_frame(tl1$daily, nrows = 70, ncols = 4)
  checkmate::expect_names(names(tl1$daily), identical.to = c("date", "t", "time_line_date", "time_line_t"))
  checkmate::expect_data_frame(tl1$time_line, nrows = 10, ncols = 2)
  checkmate::expect_names(names(tl1$time_line), identical.to = c("date", "t"))

  data("swedish_polls_curated")
  swedish_polls <- swedish_polls_curated
  expect_silent(pd <- polls_data(y = swedish_polls[,3:11],
                                 house = swedish_polls$Company,
                                 publish_date = swedish_polls$PublDate,
                                 start_date = swedish_polls$collectPeriodFrom,
                                 end_date = swedish_polls$collectPeriodTo,
                                 n = swedish_polls$n))
  expect_silent(pd <- subset_dates(pd, from = "2000-01-01", to = "2019-12-31"))
  no_date_na <- !is.na(start_dates(pd)) & !is.na(end_dates(pd))
  no_n_na <- !is.na(n(pd))

  pd2 <- subset(pd, no_date_na & no_n_na)
  expect_silent(tl2a <- time_line(x = time_range_polls(pd2), "week")) # 6030 days
  checkmate::expect_data_frame(tl2a$daily, nrows = 6557, ncol = 4)
  expect_silent(tl2b <- time_line(x = pd2, "week"))
  checkmate::expect_data_frame(tl2b$daily, nrows = 7304 + 1, ncol = 4)

  pd3 <- pd2[1]
  expect_silent(tl3a <- time_line(x = time_range_polls(pd3), time_scale = "week"))
  checkmate::expect_data_frame(tl3a$daily, nrows = 17, ncol = 4)
  checkmate::expect_subset(max(tl3a$time_line$date), choices = tl3a$daily$date)
  expect_silent(tl3b <- time_line(x = pd2, "week"))
  checkmate::expect_data_frame(tl3b$daily, nrows = 7304 + 1, ncol = 4)
  checkmate::expect_subset(max(tl3b$time_line$date), choices = tl3b$daily$date)


  skip("test month and day time lines that they work as expected.")
  skip("Split geom_latent_state to geom_latent_mean and geom_latent_state_interval.")

})


test_that("get_time_points_from_time_line works", {
  x <- 1:10
  expect_silent(tl1 <- time_line(x, time_scale = "week", start_date = "2010-01-04"))
  expect_equal(get_time_points_from_time_line(dates = as.Date(c("2010-01-04", "2010-01-05", "2010-01-10", "2010-01-11", "2010-01-18")), tl1), c(1L, 1L, 1L, 2L, 3L))
})


test_that("get_time_points_from_time_line edge case tests", {
  x <- 1:100
  expect_silent(tl1 <- time_line(x, time_scale = "week", start_date = "2010-01-01"))
  expect_equal(get_time_points_from_time_line(dates = as.Date(c("2010-01-01", "2011-11-27")), tl = tl1), c(1L, 100L))
  expect_error(get_time_points_from_time_line(dates = as.Date("2009-12-31"), tl1))
  expect_error(get_time_points_from_time_line(dates = as.Date("2011-11-28"), tl1))
})


test_that("get time_range from time_line", {
  x <- 1:10
  expect_silent(tl1 <- time_line(x, time_scale = "day", start_date = "2010-01-04"))
  tr <- time_range(c("2010-01-04", "2010-01-13"))
  expect_equal(time_range(tl1), tr)
})


test_that("time line expand", {
  x <- 1:10
  expect_silent(tl1 <- time_line(x, time_scale = "day", start_date = "2010-01-04"))

  tr <- time_range(c("2010-01-06", "2010-01-07"))
  expect_silent(tl2 <- time_line_expand(tl = tl1, time_range = tr))
  expect_identical(tl1, tl2)

  tr <- time_range(c("2010-01-01", "2010-01-15"))
  expect_silent(tl3 <- time_line_expand(tl1, tr))
  expect_equal(min(tl3$daily$t), -2)
  expect_equal(max(tl3$daily$t), max(tl1$daily$t) + 2L)

  tr <- time_range(c("2010-01-06", "2010-01-15"))
  expect_silent(tl3 <- time_line_expand(tl1, tr))
  expect_equal(min(tl3$daily$t), 1)
  expect_equal(max(tl3$daily$t), max(tl1$daily$t) + 2L)

  tr <- time_range(c("2010-01-02", "2010-01-07"))
  expect_silent(tl3 <- time_line_expand(tl1, tr))
  expect_equal(min(tl3$daily$t), -1)
  expect_equal(max(tl3$daily$t), max(tl1$daily$t))

  skip("Add tests on weeks here")
})



test_that("time_line_add_slow_scale", {
  x <- 1:50
  expect_silent(tl1 <- time_line(x, time_scale = "day", start_date = "2010-01-04"))

  expect_error(tls <- time_line_add_slow_scale(tl1, slow_scales = c("2010-01-06", "2010-01-30")))
  expect_silent(tls <- time_line_add_slow_scale(tl1, slow_scales = as.Date(c("2010-01-06", "2010-01-30"))))

  expect_silent(ts <- get_time_points_from_time_line(as.Date(c("2010-01-04", "2010-01-06", "2010-01-29" ,"2010-01-30", "2010-01-31", "2010-02-06")), tls, "time_line_s"))
  expect_equal(ts, c(1,1,2,2,3,3))

  expect_silent(tls <- time_line_add_slow_scale(tl = tl1, slow_scales = as.Date(c("2010-01-01", "2010-01-06", "2010-01-30"))))
  expect_silent(ts <- get_time_points_from_time_line(dates = as.Date(c("2010-01-04", "2010-01-06", "2010-01-29" ,"2010-01-30", "2010-01-31", "2010-02-06")), tls, "time_line_s"))
  expect_equal(ts, c(1,1,2,2,3,3) + 1L)

  expect_silent(tls <- time_line_add_slow_scale(tl = tl1, slow_scales = NULL))
  expect_silent(ts <- get_time_points_from_time_line(as.Date(c("2010-01-04", "2010-01-06", "2010-01-29" ,"2010-01-30", "2010-01-31", "2010-02-06")), tls, "time_line_s"))
  expect_equal(ts, c(1,1,1,1,1,1))

  expect_silent(tls <- time_line_add_slow_scale(tl = tl1, slow_scales = as.Date(c("2010-01-01", "2010-01-06", "2010-01-30"))))
  expect_equal(tls$slow_scales, as.Date(c("2010-01-01", "2010-01-06", "2010-01-30")))
  expect_silent(tls <- time_line_add_slow_scale(tl = tl1, slow_scales = as.Date(c("2010-01-01", "2010-01-30",  "2010-01-06"))))
  expect_equal(tls$slow_scales, as.Date(c("2010-01-01", "2010-01-06", "2010-01-30")))
  expect_silent(tls <- time_line_add_slow_scale(tl = tl1, slow_scales = NULL))
  expect_null(tls$slow_scales)

  x <- 1:50
  expect_silent(tl1 <- time_line(x, time_scale = "week", start_date = "2010-01-04"))
  expect_silent(tls <- time_line_add_slow_scale(tl = tl1, slow_scales = as.Date(c("2010-01-01", "2010-01-06", "2010-01-30", "2010-07-30"))))
  expect_silent(ts <- get_time_points_from_time_line(dates =
                as.Date(c("2010-01-04", "2010-01-29" ,"2010-01-30", "2010-01-31", "2010-02-06", "2010-08-06")), tls, "time_line_s"))
  expect_equal(ts, c(2,3,3,4,4,5))

  # Compute s_t
  ts <- stan_data_s_t(tls)
  expect_equal(ts[1:6], c(2,3,3,3,4,4))
  expect_length(ts, nrow(tls$time_line))
  tls$daily <- tls$daily[3:nrow(tls$daily),]
  ts <- stan_data_s_t(tls)
  expect_equal(ts[1:6], c(2,3,3,3,4,4))
  expect_length(ts, nrow(tls$time_line))

})

