// Built from model 6b, 8a4,
data {
  int<lower=1> T; // no of time points
  int<lower=1> N; // no of polls
  int<lower=1> L; // no of poll time points
  int<lower=1> P; // no of parties/categories
  int<lower=1> S; // no of slower moving time periods
  int<lower=1> H; // no of houses
  matrix<lower=0, upper=1>[N,P] y; // poll estimate
  matrix<lower=0, upper=1>[N,P] sigma_y; // poll_estimate standard error

  // Indicate use of measurements
  int<lower=0, upper=1> use_industry_bias;
  int<lower=0, upper=1> use_house_bias;
  int<lower=0, upper=1> use_design_effects;

  int<lower=0, upper=1> use_constrained_party_house_bias;
  int<lower=0, upper=1> use_constrained_house_house_bias;
  int<lower=0, upper=1> use_constrained_party_kappa;
  int<lower=0, upper=1> use_random_walk_kappa;

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

  // Industry bias
  real<lower=0> g[N]; // years since last election
  int<lower=1, upper=T_known + 1> next_known_state_index[N]; // The index of the next known state

  // House bias and design effects
  // slower time s and house of polls
  int<lower=1, upper=S> s_i[N];
  int<lower=1, upper=H> h_i[N];

  // The industry bias sigma_kappa prior
  // It depends on the length between known states
  real<lower=0> sigma_kappa_hyper;
  real<lower=0> sigma_beta_mu_sigma_hyper;
  real<lower=0> beta_mu_1_sigma_hyper;
  real<lower=0> sigma_beta_sigma_sigma_hyper;
  real<lower=0> beta_sigma_sigma_hyper;
  real<lower=0> kappa_1_sigma_hyper;

  // Contraint priors
  real<lower=0> kappa_sum_sigma_hyper;
  real<lower=0> beta_mu_sum_party_sigma_hyper;
  real<lower=0> beta_mu_sum_house_sigma_hyper;

  real<lower=0> g_scale;
}

transformed data {
  // Compute hyperparameter based on time_scale_length
  real<lower=0, upper=1> sigma_x_hyper = 0.25 * sqrt(time_scale_length / 30);
  real<lower=0> gs[N];
  for(i in 1:N){
    gs[i] = g[i]/g_scale;
  }
}

parameters {
  matrix<lower=0, upper=1>[T - T_known, P] x_unknown; // unknown states (proportions)
  vector<lower=0>[P] sigma_x; // dynamic movement
  matrix[use_industry_bias ? (T_known + 1) : 0, use_industry_bias ? P : 0] kappa; // Industry bias
  vector<lower=0>[use_industry_bias ? P : 0] sigma_kappa; // Industry bias effect
  real beta_mu[use_house_bias ? S : 0, use_house_bias ? H : 0, use_house_bias ? P : 0];
  real<lower=0> sigma_beta_mu[use_house_bias ? 1 : 0];
  real beta_sigma[use_design_effects ? S : 0, use_design_effects ? H : 0];
  real<lower=0> sigma_beta_sigma[use_design_effects ? 1 : 0];
}

transformed parameters {
  matrix[N, P] mu = rep_matrix(0, N, P);
  // states (proportions)
  matrix<lower=0, upper=1>[T, P] x = rep_matrix(0, T, P);
  vector[use_constrained_party_kappa ? (T_known + 1) : 0] kappa_sum_T_known_plus_1  = rep_vector(0, use_constrained_party_kappa ? (T_known + 1) : 0);
  matrix[use_constrained_party_house_bias ? S : 0, use_constrained_party_house_bias ? H : 0] beta_mu_sum_H = rep_matrix(0, use_constrained_party_house_bias ? S : 0, use_constrained_party_house_bias ? H : 0);
  matrix[use_constrained_house_house_bias ? S : 0, use_constrained_house_house_bias ? P : 0] beta_mu_sum_P = rep_matrix(0, use_constrained_house_house_bias ? S : 0, use_constrained_house_house_bias ? P : 0);

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

  // Add industry bias
  if(use_industry_bias){
    for(p in 1:P){
      for(i in 1:N) {
        if(y_missing[i, p] == 0){
            mu[i,p] = mu[i,p] + gs[i] * kappa[next_known_state_index[i], p];
          }
        }
      }
    }

  // Add house bias to mu
  if(use_house_bias){
    for(p in 1:P){
      for(i in 1:N) {
        if(y_missing[i, p] == 0){
            mu[i,p] = mu[i,p] + beta_mu[s_i[i],h_i[i],p];
          }
        }
      }
  }

  // Add soft constrain over parties for Kappa
  if(use_constrained_party_kappa){
    for(t in 1:(T_known + 1)){
      for(p in 1:P){
        kappa_sum_T_known_plus_1[t] += kappa[t,p];
      }
    }
  }

  // Add soft constrain over parties for beta_mu
  if(use_constrained_party_house_bias){
    for(s in 1:S) {
      for(h in 1:H) {
        for(p in 1:P){
            beta_mu_sum_H[s,h] += beta_mu[s,h,p];
        }
      }
    }
  }
  // Add soft constrain over houses for beta_mu
  if(use_constrained_house_house_bias){
    for(s in 1:S) {
      for(p in 1:P){
        for(h in 1:H) {
          beta_mu_sum_P[s,p] += beta_mu[s,h,p];
        }
      }
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
    // TODO: Add priors
    for(t in t_start[p]:t_end[p]) { //
      // x[t] ~ normal(x[t-1], sigma_x);
      target += normal_lpdf(x[t, p] | x[t-1, p], sigma_x[p]);
    }
  }

  // Industry bias prior
  if(use_industry_bias){
    if(use_random_walk_kappa) {
      for(p in 1:P){
        target += normal_lpdf(kappa[1, p] | 0, kappa_1_sigma_hyper);
        for(j in 2:(T_known+1)) {
           target += normal_lpdf(kappa[j, p] | kappa[j - 1, p], sigma_kappa[p]);
        }
        target += normal_lpdf(sigma_kappa[p] | 0, sigma_kappa_hyper);
      }
    } else {
      for(p in 1:P){
        for(j in 1:(T_known+1)) {
           target += normal_lpdf(kappa[j, p] | 0, sigma_kappa[p]);
        }
        target += normal_lpdf(sigma_kappa[p] | 0, sigma_kappa_hyper);
      }
    }

  }
  // Add soft constraint prior for Kappa
  if(use_constrained_party_kappa){
    for(t in 1:(T_known + 1)){
        target += normal_lpdf(kappa_sum_T_known_plus_1[t] | 0, kappa_sum_sigma_hyper);
    }
  }
  // Add soft constrain over parties for beta_mu
  if(use_constrained_party_house_bias){
    for(s in 1:S) {
      for(h in 1:H) {
        target += normal_lpdf(beta_mu_sum_H[s,h] | 0, beta_mu_sum_party_sigma_hyper);
      }
    }
  }
  // Add soft constrain over parties for beta_mu
  if(use_constrained_house_house_bias){
    for(s in 1:S) {
      for(p in 1:P) {
        target += normal_lpdf(beta_mu_sum_P[s,p] | 0, beta_mu_sum_house_sigma_hyper);
      }
    }
  }


  // House bias prior
  if(use_house_bias){
    for(p in 1:P){
      for(h in 1:H) {
        target += normal_lpdf(beta_mu[1,h,p] | 0, beta_mu_1_sigma_hyper);
        if(S > 1){
          for(s in 2:S) {
            target += normal_lpdf(beta_mu[s, h, p] | beta_mu[s - 1, h, p], sigma_beta_mu);
          }
        }
      }
    }
    target += normal_lpdf(sigma_beta_mu | 0, sigma_beta_mu_sigma_hyper);
  }
  // add soft constraint over parties
  // add soft constraint over houses

  // Design effects prior
  if(use_design_effects){
    for(h in 1:H) {
      target += normal_lpdf(beta_sigma[1,h] | 0, beta_sigma_sigma_hyper);
      if(S > 1){
        for(s in 2:S) {
          target += normal_lpdf(beta_sigma[s, h] | beta_sigma[s - 1, h], sigma_beta_sigma);
        }
      }
    }
    target += normal_lpdf(sigma_beta_sigma | 0, sigma_beta_sigma_sigma_hyper);
  }

  // Observations with and without design effects
  if(use_design_effects){
    for(p in 1:P){
      for(i in 1:N) {
        if(y_missing[i, p] == 0){
          // y[i] ~ normal(mu[i], sigma_y[i]);
          target += normal_lpdf(y[i,p] | mu[i,p], sigma_y[i,p] * exp(beta_sigma[s_i[i],h_i[i]]));
        }
      }
    }
  } else {
    for(p in 1:P){
      for(i in 1:N) {
        if(y_missing[i, p] == 0){
          // y[i] ~ normal(mu[i], sigma_y[i]);
          target += normal_lpdf(y[i,p] | mu[i,p], sigma_y[i,p]);
        }
      }
    }
  }
}

