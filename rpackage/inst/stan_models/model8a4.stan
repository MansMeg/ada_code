// Built from model 6b
data {
  int<lower=1> T; // no of time points
  int<lower=1> N; // no of polls
  int<lower=1> L; // no of poll time points
  int<lower=1> P; // no of parties/categories
  matrix<lower=0, upper=1>[N,P] y; // poll estimate
  matrix<lower=0, upper=1>[N,P] sigma_y; // poll_estimate standard error

  // missing values
  matrix<lower=0, upper=1>[N,P] y_missing; // indicator of missing values
  int<lower=1, upper=T> t_start[P]; // starting point for latent state
  int<lower=1, upper=T> t_end[P]; // end point of latent state

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
  matrix<lower=0, upper=1>[T_known, P] x_known; // known x
  real<lower=0> g[N]; // years since last election
  int<lower=1, upper=T_known + 1> next_known_state_index[N]; // The index of the next known state

  // The industry bias sigma_kappa prior
  // It depends on the length between known states
  real<lower=0> sigma_kappa_hyper;
}

transformed data {
  // Compute hyperparameter based on time_scale_length
  real<lower=0, upper=1> sigma_x_hyper = 0.25 * sqrt(time_scale_length / 30);
}

parameters {
  matrix<lower=0, upper=1>[T - T_known, P] x_unknown; // unknown states (proportions)
  vector<lower=0>[P] sigma_x; // dynamic movement
  matrix[T_known + 1, P] kappa; // Industry bias
  vector<lower=0>[P] sigma_kappa; // Industry bias effect
}

transformed parameters {
  matrix[N, P] mu = rep_matrix(0, N, P);
  // states (proportions)
  matrix<lower=0, upper=1>[T, P] x = rep_matrix(0, T, P);

  // setup x with known and unknown x
  x[x_known_t, ] = x_known;
  x[x_unknown_t, ] = x_unknown;

  // setup x with known x = 0 when parties does not exist
  for(p in 1:P){
    // we need to set values 2 steps before to 0,
    // since the value before is used as a prior
    // this could be handled in the model instead
    // If set to 0, this forces the first time step to jump from 0
    if(t_start[p] > 2){ // note: the first value is the state-space prior
      for(t in 1:(t_start[p]-2)) {
        x[t,p] = 0.0;
      }
    }
    if(t_end[p] < T){
      for(t in (t_end[p] + 1):T) {
        x[t,p] = 0.0;
      }
    }
  }

  // sum over period to handle weight periods
  for(p in 1:P){
    for(l in 1:L) {
      mu[tw_i[l], p] += tw[l] * x[tw_t[l], p];
    }
  }
}


model {
  // priors
  // x[1] ~ normal(0.5, 1);

  // sigma_x ~ normal(0, sigma_x_hyper);
  target += normal_lpdf(sigma_x | 0, sigma_x_hyper);

  // latent state
  for(p in 1:P){
    for(t in t_start[p]:t_end[p]) { //
      // x[t] ~ normal(x[t-1], sigma_x);
      target += normal_lpdf(x[t, p] | x[t-1, p], sigma_x[p]);
    }
  }

  // Measure effects
  for(p in 1:P){
    for(j in 1:(T_known+1)) {
       target += normal_lpdf(kappa[j, p] | 0 , sigma_kappa[p]);
    }
    target += normal_lpdf(sigma_kappa[p] | 0, sigma_kappa_hyper);
  }

  // Observations
  for(p in 1:P){
    for(i in 1:N) {
      if(y_missing[i, p] == 0){
        // y[i] ~ normal(mu[i], sigma_y[i]);
        target += normal_lpdf(y[i,p] | mu[i,p] + g[i] * kappa[next_known_state_index[i], p], sigma_y[i,p]);
      }
    }
  }
}

