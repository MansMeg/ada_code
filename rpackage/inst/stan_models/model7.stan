/*

*/
data {
  int<lower=1> T; // no of time points
  int<lower=1> N; // no of polls
  int<lower=1> L; // no of poll time points
  int<lower=1> P; // no of parties/categories
  matrix<lower=0, upper=1>[N,P] y; // poll estimate
  matrix<lower=0, upper=1>[N,P] sigma_y; // poll_estimate standard error

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
}

transformed data {
  // Compute hyperparameter based on time_scale_length
  real<lower=0, upper=1> sigma_x_hyper = 0.25 * sqrt(time_scale_length / 30);
}

parameters {
  matrix<lower=0, upper=1>[T - T_known, P] x_unknown; // unknown states (proportions)
  vector<lower=0>[P] sigma_x; // dynamic movement
  vector<lower=0>[T_known] V_known; // jumps at the location
  real<lower=0> nu; // jump distribution
}

transformed parameters {
  matrix[N, P] mu = rep_matrix(0, N, P);
  // states (proportions)
  matrix<lower=0, upper=1>[T, P] x = rep_matrix(0, T, P);

  // setup x with known and unknown x
  x[x_known_t, ] = x_known;
  x[x_unknown_t, ] = x_unknown;

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
  target += normal_lpdf(nu | 0, 10);
  target += gamma_lpdf(V_known | 1/nu,1/nu);
  // latent state
  
    for(i in 1:(T - T_known)) {
      
        int t = x_unknown_t[i];
        if(t>1)
          target += normal_lpdf(x[t, ] | x[t-1, ], sigma_x);
      
    }
    for(i in 1:T_known) {
        int t = x_known_t[i];
        if(t>1)
          target += normal_lpdf(x[t, ] | x[t-1, ], sqrt(V_known[i])*sigma_x);
    }

  // Observations
  for(i in 1:N) {
    for(p in 1:P){
      // y[i] ~ normal(mu[i], sigma_y[i]);
      target += normal_lpdf(y[i,p] | mu[i,p], sigma_y[i,p]);
    }
  }
}
generated quantities{
  matrix<lower=0, upper=1>[N,P] y_pred;

    for(i in 1:N) {
      for(p in 1:P){
        y_pred[i,p] = normal_rng(mu[i,p], sigma_y[i,p]);
      }
    }
}


