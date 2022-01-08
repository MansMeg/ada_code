context("polls_data")

test_that("polls_data constructor works", {
  data("swedish_polls_curated")
  expect_silent(pd <- polls_data(y = swedish_polls_curated[,3:11],
                                 house = swedish_polls_curated$Company,
                                 publish_date = swedish_polls_curated$PublDate,
                                 start_date = swedish_polls_curated$collectPeriodFrom,
                                 end_date = swedish_polls_curated$collectPeriodTo,
                                 n = swedish_polls_curated$n))
  expect_silent(pd <- subset_dates(pd, from = "2000-01-01", to = "2019-12-31"))
  expect_output(print(pd))

  # Getter and setters
  expect_equal(time_range(pd), time_range(c(from = as.Date("2000-01-01"), to = as.Date("2019-12-31"))))
  new_tr <- time_range(c(from = as.Date("2000-01-01"), to = as.Date("2019-12-31")))
  expect_silent(time_range(pd) <- new_tr)
  expect_equal(time_range(pd), new_tr)

  expect_equal(end_dates(pd), pd$poll_info$.end_date)
  expect_equal(end_dates(pd), end_date(pd))
  expect_equal(start_dates(pd), pd$poll_info$.start_date)
  expect_equal(start_dates(pd), start_date(pd))
  expect_equal(publish_dates(pd), pd$poll_info$.publish_date)
  expect_equal(publish_dates(pd), publish_date(pd))

  expect_silent(suppressWarnings(plot(pd, "S")))

})



