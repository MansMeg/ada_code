library(ada)
library(rstan)
library(SwedishPolls)

start_time <- Sys.time()

# Read cfg ----
print("Read cfg")
cfg <- yaml::read_yaml("run_ada/ada_config.yml")
cat(yaml::as.yaml(cfg))


# Set model data parameters/settings ----
print("Set data parameters/settnings")
if(is.null(cfg$data_arguments$dates_model_start)){
  dates_model_start <- as.Date("2002-01-01")
} else {
  dates_model_start <- as.Date(cfg$data_arguments$dates_model_start)
}
if(is.null(cfg$data_arguments$dates_model_end)){
  end_date <- Sys.Date() + 60L
  dates_model_end <- as.Date(paste0(lubridate::year(end_date),"-", lubridate::month(end_date), "-01"))
} else {
  dates_model_end <- as.Date(cfg$data_arguments$dates_model_end)
}
if(is.null(cfg$data_arguments$dates_polls_start)){
  dates_polls_start <- dates_model_start
} else {
  dates_polls_start <- as.Date(cfg$data_arguments$dates_polls_start)
}
if(is.null(cfg$data_arguments$dates_polls_end)){
  dates_polls_end <- dates_model_end
} else {
  dates_polls_end <- as.Date(cfg$data_arguments$dates_polls_end)
}
if(is.null(cfg$data_arguments$dates_known_state_end)){
  dates_known_state_end <- dates_model_end
} else {
  dates_known_state_end <- as.Date(cfg$data_arguments$dates_known_state_end)
}
time_scale <- "week"
parties <- c("M", "L", "C", "KD", "S", "V", "MP", "SD")


## Set model arguments
print("Set model arguments")
stan_model <- cfg$model_arguments$stan_model
hyper_parameters <- cfg$model_arguments$hyper_parameters
cache_dir <- "tmp_ada_cache"
output_folder <- "tmp_ada_model_output"

set.seed(as.integer(cfg$model_arguments$seed))
options(mc.cores = parallel::detectCores())

## Set stan arguments
print("Set stan arguments")
if(!ada:::on_github_actions()){
  total_posterior_draws <- 1000L
  stan_arguments <- cfg$model_arguments$stan_arguments
  if(is.null(stan_arguments)){
    stan_arguments <- list(warmup = 1000,
                           thin = 4,
                           chains = 4)
    stan_arguments$control <- list(adapt_delta = 0.95)
  }
  stan_arguments$iter <- round(stan_arguments$warmup + stan_arguments$thin * (total_posterior_draws / stan_arguments$chains))
} else {
  # Run test on github actions - check that the code compile and runs
  stan_arguments <- list(warmup = 1L,
                         thin = 1L,
                         chains = 1L,
                         iter = 2L)
}

# Setup data ----
print("Setup data")
## Download data from github repo sweden polls
sp <- SwedishPolls::get_polls()
sp <- curate_swedish_polls(sp)
pd <- polls_data(y = sp[, parties],
                 house = sp$house,
                 publish_date = sp$PublDate,
                 start_date = sp$collectPeriodFrom,
                 end_date = sp$collectPeriodTo,
                 n = sp$n)
pd <- subset_dates(pd, from = dates_polls_start, to = dates_polls_end)
pd <- pd[complete_poll_info(pd)]

# Remove houses with less than 10 polls
house_freq <- table(houses(pd))
pd <- pd[houses(pd) %in% names(house_freq[house_freq >= 10])]
pd$poll_info$.house <- as.factor(as.character(pd$poll_info$.house))

# Elections
data("swedish_elections")
ed <- swedish_elections
ed$date <- ed$PublDate
ed <- ed[ed$date <= dates_known_state_end, ]
ed <- ed[order(ed$date), ]
start_i <- max(1, min(which(ed$date >= dates_model_start)) - 1)
ed <- ed[start_i:nrow(ed), ]

# Set slow time scale to election period
slow_scales <- ed$date[-1]

# Run model
print("Run model")
rstan::rstan_options(auto_write = TRUE)
model_start_time <- Sys.time()
pop  <- poll_of_polls(y = parties,
                      model_time_range = time_range(c(from = dates_model_start, to = dates_model_end)),
                      model = stan_model,
                      polls_data = pd,
                      time_scale = time_scale,
                      known_state = ed,
                      slow_scales = slow_scales,
                      hyper_parameters = hyper_parameters,
                      warmup = stan_arguments$warmup,
                      thin = stan_arguments$thin,
                      iter = stan_arguments$iter,
                      chains = stan_arguments$chains,
                      control = stan_arguments$control,
                      cache_dir = cache_dir)

# Write model output locally ----
print("Write model output locally")
job_id <- cfg$job_id
rds_file_path <- file.path(getwd(), paste0(pop$sha, ".rds"))
saveRDS(pop, file = rds_file_path)

## Print outputs
warnings()

print(sprintf("job_id = %s", job_id))

# Runtime
runtime <-  list(start = start_time,
                 end = Sys.time())
cat("Run time:\n")
print(runtime$end - runtime$start)


