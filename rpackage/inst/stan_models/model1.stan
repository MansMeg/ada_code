
data {
  int<lower=1> T;
  int<lower=1> N;
  real<lower=0,upper=1> y[N];
  real t_obs[N];
}

parameters {
  vector[T] x; // states (proportions)
  real<lower=0> sigma_x;
  real<lower=0> sigma_y;
}



model {
  // priors
  x[1] ~ normal(0, 1)
  sigma_x ~ cauchy(0, 1);
  sigma_y ~ cauchy(0, 1);

  // state
  for(t in 2:T) {
    x[t] ~ normal(x[t-1], sigma_x);
  }
  // obs
  for(n in 1:N) {
    y[n] ~ normal(x[t_obs[n]], sigma_y);
  }
}

//generated quantities {
//  pred_x = normal_rng(x[N], sigma_x);
//  pred_y = normal_rng(pred_x, sigma_y);
//}
