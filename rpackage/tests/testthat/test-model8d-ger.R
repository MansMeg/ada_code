context("model8d-ger")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)

test_that("test model 8d: Design effect in Germany 2008-", {
  skip_if_not(adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  start_date <- as.Date("2008-01-01")
  end_date <- as.Date("2020-01-01")
  parties <- c("CDU/CSU", "SPD", "GRUNE", "FDP", "LINKE", "AfD")
  data("german_elections")
  data("german_polls_curated")


  pd <- polls_data(y = german_polls_curated[, parties],
                   house = german_polls_curated$house,
                   publish_date = german_polls_curated$PublDate,
                   start_date = german_polls_curated$collectPeriodFrom,
                   end_date = german_polls_curated$collectPeriodTo,
                   n = german_polls_curated$n)
  pd <- subset_dates(pd, from = start_date, to = end_date)
  pd <- pd[complete_poll_info(pd)]

  house_freq <- table(houses(pd))
  pd <- pd[houses(pd) %in% names(house_freq[house_freq >= 10])]

  ed <- german_elections
  ed$date <- ed$PublDate

  print_plt <- TRUE
  tmp_folder <- "tmp_figs/ger_8d"
  time_scale <- "week"
  set.seed(4711)

  pd$poll_info$.house <- as.factor(as.character(pd$poll_info$.house))
  expect_message(
    sd <- stan_polls_data(x = pd,
                          time_scale = time_scale,
                          y_name = parties,
                          model = "model8d",
                          slow_scales = ed$date[-1],
                          hyper_parameters = list(use_industry_bias = 1L,
                                                  use_house_bias = 1L,
                                                  use_design_effects = 1L),
                          known_state = ed)
  )

  expect_silent(pop8d_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8d <-  poll_of_polls(y = parties,
                                                model = "model8d",
                                                polls_data = pd,
                                                time_scale = time_scale,
                                                known_state = ed,
                                                slow_scales = ed$date[-1],
                                                hyper_parameters = list(use_industry_bias = 1L,
                                                                        use_house_bias = 1L,
                                                                        use_design_effects = 1L),
                                                warmup = 2000,
                                                iter = 2250,
                                                chains = 4,
                                                cache_dir = "tmp_cache")
                      )
                    )
                  )
  )
  # Warning messages:
  #   1: There were 250 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
  # http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded

  # Check that it returs a pop object.
  expect_s3_class(pop8d, "poll_of_polls")

  # Test that the x[known] has no sd while the unknows has
  expect_silent(ls <- latent_state(pop8d))

  if(print_plt) dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE)

  # Check that the pop object can be plotted
  for(party in parties){
    # party <- "CDU/CSU"
    expect_silent(
      plt <- plot(pop8d, party) + geom_known_state(pop8d, party) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, make.names(paste0("latent_2010_",party,".jpg"))), width = 6, height = 3)

    expect_silent(
      plt <- plot_poll_of_polls(pop8d, party, from = "2017-01-01") + geom_known_state(pop8d, party) + ggplot2::theme_bw()
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, make.names(paste0("latent_2017_",party,".jpg"))), width = 6, height = 3)
  }

  houses <- levels(pd$poll_info$.house)
  for(j in seq_along(houses)){
    # j <- 1
    house <- houses[j]
    expect_silent(
      plt <- plot_parameters_areas(pop8d,
                                   paste0("beta_sigma[",1:(length(ed$date[-1])+1),",",j,"]"),
                                   paste0("beta_sigma[",c(lubridate::year(ed$date[-1]), "2022"),",",house,"]"),
                                   title = paste0("Design effect of ", house))
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("beta_sigma_",make.names(house),".jpg")), width = 6, height = 4)
  }

  expect_silent(
    plt <- plot_parameters_areas(pop8d, paste0("sigma_x[", seq_along(pop8d$y), "]"), pop8d$y, title = "Posteriord distribution for sigma_x")
  )
  if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, paste0("sigma_x.jpg")), width = 4, height = 3)

  for(i in seq_along(parties)){
    # i <- 1
    expect_silent(
      plt <- plot_parameters_areas(pop8d, paste0("kappa[", 1:4, ",",i,"]"), paste0("kappa[", c(substr(ed$date[-1],1,4), "2022"), ",",parties[i],"]"), title = paste0("Posteriord distribution for kappas (", parties[i], ")"))
    )
    if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, make.names(paste0("kappa_", parties[i],".jpg"))), width = 4, height = 3)
  }

  houses <- levels(pd$poll_info$.house)
  for(i in seq_along(parties)){
    # i <- 1
    party <- parties[i]
    for(j in seq_along(houses)){
      # j <- 1
      house <- houses[j]
      expect_silent(
        plt <- plot_parameters_areas(pop8d,
                                     paste0("beta_mu[",1:(length(ed$date[-1])+1),",",j,",",i,"]"),
                                     paste0("beta_mu[",c(lubridate::year(ed$date[-1]), "2022"),",",house,",",party,"]"),
                                     title = paste0("House bias of ", house, " for ", party))
      )
      if(print_plt) ggplot2::ggsave(plt, filename = file.path(tmp_folder, make.names(paste0("beta_mu_",party,"_",make.names(house),".jpg"))), width = 4, height = 3)
    }
  }

  write_ada_json(x = pop8d, "tmp_output/")
  write_latent_mean_csv(pop8d, paste0("tmp_output/latent_mean_",pop8d$model,"_",substr(pop8d$sha,1,6),".csv"))

})


