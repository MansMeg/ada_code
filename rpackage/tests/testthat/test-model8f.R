context("model8f")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)
# options(mc.cores = parallel::detectCores())

test_that("Test model 8e and 8f are identical wrt g", {
  # First we test that we get a similar result with 8d as with 8a4
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 150,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)

  expect_message(
    suppressWarnings(
      sdf <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8f",
                            known_state = known_state,
                            hyper_parameters = list(sigma_kappa_hyper = 0.01))
    )
  )

  expect_message(
    suppressWarnings(
      sde <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8e",
                            known_state = known_state,
                            hyper_parameters = list(sigma_kappa_hyper = 0.01))
    )
  )

  expect_equal(sde$stan_data$g, sdf$stan_data$g_i)
  expect_equal(sde$stan_data$g_scale, sdf$stan_data$g_scale)

  # test that g_t[collection_midpoint] is equal to g_i
  dates <- collection_midpoint_dates(spd)
  ts <- adapop:::get_time_points_from_time_line(dates, tl = sdf$time_line)
  expect_equal(sdf$stan_data$g_i, sdf$stan_data$g_t[ts])
  expect_equal(sdf$stan_data$next_known_state_poll_index, sdf$stan_data$next_known_state_t_index[ts])
})



test_that("Test model 8e and 8f are identical", {
  # First we test that we get a similar result with 8d as with 8a4
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")


  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 150,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)

  expect_message(
    suppressWarnings(
      sd <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8f",
                            known_state = known_state,
                            hyper_parameters = list(sigma_kappa_hyper = 0.01))
    )
  )

  # Simulate new with industry bias
  kappa_x3 <- 0.05
  kappa_x4 <- -0.05
  spd$y[,"x3"] <-  spd$y[,"x3"] + sd$stan_data$g_i * kappa_x3
  spd$y[,"x4"] <-  spd$y[,"x4"] + sd$stan_data$g_i * kappa_x4

  # Check that we get identical lpd
  cfg <-  list(sigma_kappa_hyper = 0.01,
               use_industry_bias = 1L,
               use_house_bias = 1L,
               use_design_effects = 1L,
               g_scale = 4,
               use_constrained_house_house_bias = 1L,
               use_constrained_party_house_bias = 1L,
               use_constrained_party_kappa = 1L)

  expect_silent(pop8e_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8e <- poll_of_polls(y = parties,
                                               model = "model8e",
                                               polls_data = spd,
                                               time_scale = time_scale,
                                               known_state = known_state,
                                               hyper_parameters = cfg,
                                               warmup = 0,
                                               iter = 5,
                                               chains = 1,
                                               cache_dir = NULL)
                      )
                    )
                  )
  )
  expect_silent(pop8f_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8f <- poll_of_polls(y = parties,
                                                model = "model8f",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = cfg,
                                                warmup = 0,
                                                iter = 5,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  pn8f <- parameter_names(pop8f)
  pn8e <- parameter_names(pop8e)
  checkmate::expect_subset(pn8e, pn8f)

  no_up <- get_num_upars(pop8e)
  expect_equal(no_up, get_num_upars(pop8f))

  par_values <- rep(0, no_up)
  lp1_8f <- log_prob(pop8f, par_values)
  lp1_8e  <- log_prob(pop8e, par_values)
  expect_equal(lp1_8f, lp1_8e)

  set.seed(4712)
  par_values <- rnorm(no_up)
  lp2_8f <- log_prob(pop8f, par_values)
  lp2_8e  <- log_prob(pop8e, par_values)
  expect_equal(lp2_8f, lp2_8e)

})


test_that("Test stan_data bug", {
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")


  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(20, 40, 60, 80, 100)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 500,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)

  # Simulate new with industry bias
  expect_warning(
    suppressMessages(
        sd <- stan_polls_data(x = spd,
                              time_scale = time_scale,
                              y_name = c("x3", "x4"),
                              model = "model8f",
                              known_state = known_state,
                              hyper_parameters = list(sigma_kappa_hyper = 0.001))
      )
  )

})


test_that("Test predictive kappa_next", {
  skip_if_not(adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")


  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 500,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)

  # Simulate new with industry bias
  suppressMessages(
    suppressWarnings(
      sd <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8f",
                            known_state = known_state,
                            hyper_parameters = list(sigma_kappa_hyper = 0.001))
  ))

  # Simulate new with kappa = 0.05 per year
  kappa_x3_s1 <- 0.05
  kappa_x3_s2 <- 0.025
  kappa_x3_s3 <- 0
  kappa_x4_s1 <- -0.05
  kappa_x4_s2 <- -0.025
  kappa_x4_s3 <- 0
  is_s1 <- collection_midpoint_dates(spd) <= known_state$date[1]
  is_s2 <- collection_midpoint_dates(spd) <= known_state$date[2] & collection_midpoint_dates(spd) > known_state$date[1]
  is_s3 <- collection_midpoint_dates(spd) > known_state$date[2]

  spd$y[is_s1,"x3"] <-  spd$y[is_s1,"x3"] + sd$stan_data$g_i[is_s1] * kappa_x3_s1
  spd$y[is_s2,"x3"] <-  spd$y[is_s2,"x3"] + sd$stan_data$g_i[is_s2] * kappa_x3_s2
  spd$y[is_s3,"x3"] <-  spd$y[is_s3,"x3"] + sd$stan_data$g_i[is_s3] * kappa_x3_s3
  spd$y[is_s1,"x4"] <-  spd$y[is_s1,"x4"] + sd$stan_data$g_i[is_s1] * kappa_x4_s1
  spd$y[is_s2,"x4"] <-  spd$y[is_s2,"x4"] + sd$stan_data$g_i[is_s2] * kappa_x4_s2
  spd$y[is_s3,"x4"] <-  spd$y[is_s3,"x4"] + sd$stan_data$g_i[is_s3] * kappa_x4_s3

  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 1L,
               use_house_bias = 1L,
               use_design_effects = 1L,
               use_constrained_house_house_bias = 1L,
               use_constrained_party_house_bias = 1L,
               use_constrained_party_kappa = 1L,
               estimate_kappa_next = 0L)

  expect_silent(pop8f_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8f1_0 <- poll_of_polls(y = parties,
                                               model = "model8f",
                                               polls_data = spd,
                                               time_scale = time_scale,
                                               known_state = known_state,
                                               hyper_parameters = cfg,
                                               warmup = 1000,
                                               iter = 1500,
                                               thin = 1,
                                               chains = 1,
                                               cache_dir = NULL)
                      )
                    )
                  )
  )

  pop <- pop8f1_0
  no_draws <- pop$stan_arguments$iter - pop$stan_arguments$warmup
  expect_equivalent(extract(pop, "kappa[3,1]")[[1]], rep(0, no_draws))
  expect_equivalent(extract(pop, "kappa[3,2]")[[1]], rep(0, no_draws))

  plt_x3 <- plot_poll_of_polls(pop, "x3") + geom_known_state(pop, "x3") + geom_pop_line(pop, txdf[["x3"]]) + ggplot2::theme_bw()
  plt_x4 <- plot_poll_of_polls(pop, "x4") + geom_known_state(pop, "x4") + geom_pop_line(pop, txdf[["x4"]]) + ggplot2::theme_bw()

  plt_x3_k <- plot_parameters_areas(pop, c("kappa[1,1]", "kappa[2,1]"))
  plt_x4_k <- plot_parameters_areas(pop, c("kappa[1,2]", "kappa[2,2]"))
  plt1 <- plot_parameters_areas(pop, c("kappa_next_pred[1]", "kappa_next_pred[2]"))

  x <- extract(pop, "x")[[1]]
  kappa_next_pred <- extract(pop, "kappa_next_pred")[[1]]
  gs_t <- pop$stan_data$stan_data$g_t / pop$stan_data$stan_data$g_scale
  next_known_state_t_index <- pop$stan_data$stan_data$next_known_state_t_index
  T_known <- pop$stan_data$stan_data$T_known
  T <- pop$stan_data$stan_data$T
  P <- pop$stan_data$stan_data$P
  x_pred <- x
  for(t in 1:T){
    if(next_known_state_t_index[t] == (T_known + 1)){
      for(p in 1:P){
        x_pred[,t,p] <- x[,t,p] - gs_t[t] * kappa_next_pred[,p]
        print(min(x_pred[,t,p]))
        print(max(x_pred[,t,p]))
      }
    }
  }
  x_pred_stan <- extract(pop, "x_pred")[[1]]

  ls <- latent_state(pop)
  dimnames(x_pred_stan) <- dimnames(ls$latent_state)
  dimnames(x_pred) <- dimnames(ls$latent_state)
  dimnames(x) <- dimnames(ls$latent_state)

  plt_x3 <- plot_poll_of_polls(pop, "x3", include_latent_state = FALSE) + geom_known_state(pop, "x3") + geom_pop_line(pop, txdf[["x3"]]) + ggplot2::theme_bw()

  ls$latent_state <- x
  plt_x3_none <- plt_x3 + geom_latent_state(ls[, ,"x3"])
  ls$latent_state <- x_pred
  plt_x3_man <- plt_x3 + geom_latent_state(ls[, ,"x3"])
  ls$latent_state <- x_pred_stan
  plt_x3_stan <- plt_x3 + geom_latent_state(ls[, ,"x3"])


  plt_x4 <- plot_poll_of_polls(pop, "x4", include_latent_state = FALSE) + geom_known_state(pop, "x4") + geom_pop_line(pop, txdf[["x4"]]) + ggplot2::theme_bw()

  ls$latent_state <- x
  plt_x4_none <- plt_x4 + geom_latent_state(ls[, ,"x4"])
  ls$latent_state <- x_pred
  plt_x4_man <- plt_x4 + geom_latent_state(ls[, ,"x4"])
  ls$latent_state <- x_pred_stan
  plt_x4_stan <- plt_x4 + geom_latent_state(ls[, ,"x4"])


  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 1L,
               use_house_bias = 1L,
               use_design_effects = 1L,
               use_constrained_house_house_bias = 1L,
               use_constrained_party_house_bias = 1L,
               use_constrained_party_kappa = 1L,
               estimate_kappa_next = 0L,
               use_ar_kappa = 1L,
               estimate_alpha_kappa = 1L)

  expect_silent(pop8f_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8f2 <- poll_of_polls(y = parties,
                                               model = "model8f",
                                               polls_data = spd,
                                               time_scale = time_scale,
                                               known_state = known_state,
                                               hyper_parameters = cfg,
                                               warmup = 1000,
                                               iter = 1500,
                                               thin = 1,
                                               chains = 1,
                                               cache_dir = NULL)
                      )
                    )
                  )
  )

  pop <- pop8f2
  plt_x3 <- plot_parameters_areas(pop, c("kappa[1,1]", "kappa[2,1]"))
  plt_x4 <- plot_parameters_areas(pop, c("kappa[1,2]", "kappa[2,2]"))
  plt_alpha_kappa <- plot_parameters_areas(pop, c("alpha_kappa[1]"))
  plt1 <- plot_parameters_areas(pop, c("kappa_next_pred[1]", "kappa_next_pred[2]"))

  x <- extract(pop, "x")[[1]]
  kappa_next_pred <- extract(pop, "kappa_next_pred")[[1]]
  gs_t <- pop$stan_data$stan_data$g_t / pop$stan_data$stan_data$g_scale
  next_known_state_t_index <- pop$stan_data$stan_data$next_known_state_t_index
  T_known <- pop$stan_data$stan_data$T_known
  T <- pop$stan_data$stan_data$T
  P <- pop$stan_data$stan_data$P
  x_pred <- x
  for(t in 1:T){
    if(next_known_state_t_index[t] == (T_known + 1)){
      for(p in 1:P){
        x_pred[,t,p] <- x[,t,p] - gs_t[t] * kappa_next_pred[,p]
        print(min(x_pred[,t,p]))
        print(max(x_pred[,t,p]))
      }
    }
  }
  x_pred_stan <- extract(pop, "x_pred")[[1]]

  ls <- latent_state(pop)
  dimnames(x_pred_stan) <- dimnames(ls$latent_state)
  dimnames(x_pred) <- dimnames(ls$latent_state)
  dimnames(x) <- dimnames(ls$latent_state)

  plt_x3 <- plot_poll_of_polls(pop, "x3", include_latent_state = FALSE) + geom_known_state(pop, "x3") + geom_pop_line(pop, txdf[["x3"]]) + ggplot2::theme_bw()

  ls$latent_state <- x
  plt_x3_none <- plt_x3 + geom_latent_state(ls[, ,"x3"])
  ls$latent_state <- x_pred
  plt_x3_man <- plt_x3 + geom_latent_state(ls[, ,"x3"])
  ls$latent_state <- x_pred_stan
  plt_x3_stan <- plt_x3 + geom_latent_state(ls[, ,"x3"])

  plt_x4 <- plot_poll_of_polls(pop, "x4", include_latent_state = FALSE) + geom_known_state(pop, "x4") + geom_pop_line(pop, txdf[["x4"]]) + ggplot2::theme_bw()

  ls$latent_state <- x
  plt_x4_none <- plt_x4 + geom_latent_state(ls[, ,"x4"])
  ls$latent_state <- x_pred
  plt_x4_man <- plt_x4 + geom_latent_state(ls[, ,"x4"])
  ls$latent_state <- x_pred_stan
  plt_x4_stan <- plt_x4 + geom_latent_state(ls[, ,"x4"])

})

