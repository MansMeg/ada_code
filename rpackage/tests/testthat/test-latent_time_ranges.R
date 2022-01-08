context("latent_time_range")

test_that("time_line constructor works", {

  y <- c("a", "b", "c", "d")
  mtr <- time_range(c("2010-01-01", "2010-12-31"))
  ltr_list <- list(a = list(from = "2010-05-01", to = "2010-10-01"),
                   b = list(from = "2010-04-01"),
                   d = list(to = "2010-05-01"))
  expect_silent(assert_latent_time_range_list(ltr_list))

  expect_silent(ltr <- setup_latent_time_ranges(ltr_list, y, mtr))

  expect_silent(assert_latent_time_ranges(ltr, y))
  expect_silent(assert_latent_time_ranges(ltr, y, mtr))

  mtr2 <- time_range(c("2010-06-01", "2010-07-01"))
  expect_error(assert_latent_time_ranges(ltr, y, mtr2), regexp = "'d'")

  y <- c("a", "b")
  mtr <- time_range(c("2010-01-01", "2010-12-31"))
  expect_silent(assert_latent_time_range_list(ltr_list))
  expect_error(assert_latent_time_ranges(ltr, y, mtr2))
  expect_warning(ltr <- setup_latent_time_ranges(x = ltr_list, y, mtr))

})



