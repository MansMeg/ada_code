if(0){
  library(adapop)
  library(rstan)
  start_date <- as.Date("2010-01-01")
  end_date <- as.Date("2021-06-30")
  parties <- c("L","S","V")
  data("swedish_elections")
  data("swedish_polls_curated")
  
  print_plt <- TRUE
  tmp_folder <- "tmp_figs/swe_10e"
  
  pd <- polls_data(y = swedish_polls_curated[, parties],
                   house = swedish_polls_curated$house,
                   publish_date = swedish_polls_curated$PublDate,
                   start_date = swedish_polls_curated$collectPeriodFrom,
                   end_date = swedish_polls_curated$collectPeriodTo,
                   n = swedish_polls_curated$n)
  pd <- subset_dates(pd, from = start_date, to = end_date)
  pd <- pd[complete_poll_info(pd)]
  
  house_freq <- table(houses(pd))
  pd <- pd[houses(pd) %in% names(house_freq[house_freq >= 10])]
  
  ed <- swedish_elections
  ed$date <- ed$PublDate
  
  time_scale <- "week"
  set.seed(4711)
  
  pd$poll_info$.house <- as.factor(as.character(pd$poll_info$.house))
  sd <- stan_polls_data(x = pd,
                          time_scale = time_scale,
                          y_name = parties,
                          model = "model11b",
                          slow_scales = ed$date[-1],
                          hyper_parameters = list(use_industry_bias = 1L,
                                                  use_house_bias = 1L,
                                                  use_design_effects = 1L,
                                                  use_constrained_party_house_bias = 0L,
                                                  use_constrained_house_house_bias = 0L,
                                                  use_constrained_party_kappa      = 0L,
                                                  use_ar_kappa  = 1L,
                                                  estimate_alpha_kappa = 1L,
                                                  use_latent_state_version= 2L),
                          known_state = ed)
  options(mc.cores = 4)
  rstan_options(auto_write = TRUE)
  pop11b_out <-  poll_of_polls(y = parties,
                              model = "model11b",
                              polls_data = pd,
                              time_scale = time_scale,
                              known_state = ed,
                              slow_scales = ed$date[-1],
                              hyper_parameters = list(use_industry_bias = 1L,
                                                      use_house_bias = 1L,
                                                      use_design_effects = 1L,
                                                      use_constrained_party_house_bias = 0L,
                                                      use_constrained_house_house_bias = 0L,
                                                      use_constrained_party_kappa      = 0L,
                                                      use_ar_kappa  = 1L,
                                                      estimate_alpha_kappa = 1L,
                                                      use_latent_state_version= 2L),
                              warmup = 1000,
                              iter   = 1500,
                              chains = 4)
  
  
  print_plt = F
  for(party in parties){
     
      plt <- plot_poll_of_polls(pop11b_out, party, from = "2018-01-01") + geom_known_state(pop11b_out, party) + ggplot2::theme_bw()
    
  }
  plt
  plt <- plot_parameters_areas(pop11b_out, paste0("alpha_kappa[", 1, "]"), pop10d_out$y, title = "Posteriord distribution for tau_x")
  
  plt <- plot_parameters_areas(pop11b_out, paste0("tau_x[", seq_along(pop10d_out$y), "]"), pop10d_out$y, title = "Posteriord distribution for tau_x")
}
  