context("model_config")

test_that("model_config works", {

  cfg1 <- list("sigma_kappa_hyper" = 0.01)
  cfg2 <- list("sigma_kappa_hyper" = 0.01,
               use_industry_bias = 0,
               use_house_bias = 0,
               use_design_effects = 1L)
  cfg3 <- list("sigma_kappa_hyper" = 0.01,
               use_house_bias = 0L,
               use_industry_bias = 0L,
               use_design_effects = 0L)
  cfg5 <- list("sigma_kappa_hyper" = 0.01,
               use_house_bias = 0L,
               use_industry_bias = 0L,
               use_design_effects = 0L,
               g_scale = 4)

  expect_message(mc1 <- model_config(model = "model8d", cfg1))
  expect_message(mc2 <- model_config(model = "model8d", NULL))
  expect_message(mc3 <- model_config(model = "model8d", cfg2))
  expect_message(mc4 <- model_config(model = "model8d", cfg3))
  expect_error(model_config(model = "model99"))
  expect_identical(mc1, mc4)

  expect_message(mc5 <- model_config(model = "model8d3", cfg5))

})



test_that("use_..._bias is needed for use_constrained_..._bias", {
  # model_arguments("model8d3")
  cfg1 <- list("sigma_kappa_hyper" = 0.01,
               use_house_bias = 0L,
               use_industry_bias = 1L,
               use_design_effects = 0L,
               use_design_effects = 0L,
               use_constrained_party_kappa = 1L,
               g_scale = 4)
  expect_message(mc1 <- model_config(model = "model8d3", cfg1))

  cfg1$use_industry_bias <- 0L
  expect_error(mc1 <- suppressMessages(model_config(model = "model8d3", cfg1)))

  cfg1 <- list(use_house_bias = 1L,
               use_constrained_house_house_bias = 1L,
               g_scale = 4)
  expect_message(mc1 <- model_config(model = "model8d3", cfg1))

  cfg1$use_house_bias <- 0L
  expect_error(mc1 <- suppressMessages(model_config(model = "model8d3", cfg1)))


  cfg1 <- list(use_house_bias = 1L,
               use_constrained_party_house_bias = 1L,
               g_scale = 4)
  expect_message(mc1 <- model_config(model = "model8d3", cfg1))

  cfg1$use_house_bias <- 0L
  expect_error(mc1 <- suppressMessages(model_config(model = "model8d3", cfg1)))

})


test_that("g_scale is computed correctly", {
  cfg1 <- list(use_house_bias = 1L)
  expect_error(mc1 <- suppressMessages(model_config(model = "model8d3", cfg1)))

  stan_data <- list(g = rep(1:5,3), next_known_state_index = c(rep(1,5), rep(2,5), rep(3,5)))
  expect_message(mc1 <- model_config(model = "model8d3", cfg1, stan_data))
  expect_equal(mc1$g_scale, 5)
})
