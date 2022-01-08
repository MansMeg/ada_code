
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
}

transformed data {
  // Compute hyperparameter based on time_scale_length
  real<lower=0, upper=1> sigma_x_hyper = 0.25 * sqrt(time_scale_length / 30);
}

parameters {
  vector<lower=0, upper=1>[T] x; // states (proportions)
  real<lower=0> sigma_x; // dynamic movement
  real<lower=1> nu;  // degrees of freedom or shape
}

transformed parameters {
  vector[N] mu = rep_vector(0, N);
  // sum over period
  for(l in 1:L) {
    // mu[tw_i[l]] = mu[tw_i[l]] + tw[l] * x[tw_t[l]];
    mu[tw_i[l]] += tw[l] * x[tw_t[l]];
  }
}


model {
  // priors
  x[1] ~ normal(0, 1);
  sigma_x ~ normal(0, sigma_x_hyper);
  nu ~ gamma(2, 0.1); // orgiginal

  // state
  for(t in 2:T) {
    x[t] ~ student_t(nu, x[t-1], sigma_x);
  }
  // obs
  for(i in 1:N) {
    y[i] ~ normal(mu[i], sigma_y[i]);
  }
}

