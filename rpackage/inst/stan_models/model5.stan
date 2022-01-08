
data {
  int<lower=1> T; // no of time points
  int<lower=1> N; // no of polls
  int<lower=1> L; // no of poll time points
  vector<lower=0, upper=1>[N] y; // poll estimate
  vector<lower=0, upper=1>[N] sigma_y; // poll_estimate standard error

  // time weights
  real<lower=0, upper=1> tw[L];
  int<lower=1> tw_t[L]; // time point (t) of time weights
  int<lower=1> tw_i[L]; // poll idx of tw

  // Time scale length (month = 30, week = 7, day = 1)
  real time_scale_length;

  // known states
  int<lower=0, upper=T> T_known; // no of known latent states
  int<lower=1> x_known_t[T_known]; // time points where x is known
  int<lower=1> x_unknown_t[T - T_known]; // time points where x is known
  vector<lower=0, upper=1>[T_known] x_known; // known x
}

transformed data {
  // Compute hyperparameter based on time_scale_length
  real<lower=0, upper=1> sigma_x_hyper = 0.25 * sqrt(time_scale_length / 30);
}

parameters {
  vector<lower=0, upper=1>[T - T_known] x_unknown; // unknown states (proportions)
  real<lower=0> sigma_x; // dynamic movement
}

transformed parameters {
  vector[N] mu = rep_vector(0, N);
  vector<lower=0, upper=1>[T] x; // states (proportions)

  // setup x
  x[x_known_t] = x_known;
  x[x_unknown_t] = x_unknown;

  // sum over period to handle weight periods
  for(l in 1:L) {
    mu[tw_i[l]] += tw[l] * x[tw_t[l]];
  }
}


model {
  // priors
  // x[1] ~ normal(0.5, 1);

  // sigma_x ~ normal(0, sigma_x_hyper);
  target += normal_lpdf(sigma_x | 0, sigma_x_hyper);

  // latent state
  for(t in 2:T) {
    // x[t] ~ normal(x[t-1], sigma_x);
    target += normal_lpdf(x[t] | x[t-1], sigma_x);
  }

  // Observations
  for(i in 1:N) {
    // y[i] ~ normal(mu[i], sigma_y[i]);
    target += normal_lpdf(y[i] | mu[i], sigma_y[i]);
  }
}

