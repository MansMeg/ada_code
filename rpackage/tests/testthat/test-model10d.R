library(adapop)

if(0){

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


sd <- stan_polls_data(x = spd,
                       time_scale = time_scale,
                       y_name = c("x3", "x4"),
                       model = "model10d",
                       known_state = known_state,
                       hyper_parameters = list(sigma_kappa_hyper = 0.001))

# Simulate new with kappa = 0.02 per year
kappa_x3 <- 0.05
kappa_x4 <- -0.05
spd$y[,"x3"] <-  spd$y[,"x3"] + sd$stan_data$g * kappa_x3
spd$y[,"x4"] <-  spd$y[,"x4"] + sd$stan_data$g * kappa_x4
pop10  <- poll_of_polls(y = parties,
                        model = "model10d",
                        polls_data = spd,
                        time_scale = time_scale,
                        known_state = known_state,
                        hyper_parameters = list(sigma_kappa_hyper = 0.02),
                        warmup = 2000,
                        iter = 2250,
                        chains = 4,
                        cache_dir = "tmp_cache")

}
