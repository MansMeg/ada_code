/*

*/
data {
  int<lower=1> T; // no of time points
  int<lower=1> N; // no of polls
  int<lower=1> L; // no of poll time points
  int<lower=1> P; // no of parties/categories
  matrix[P,P] Transformation; // matrix for moving between sum to one space to regular latent space
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
  
  matrix[P ,P-1] X_to_Z = Transformation[2:P,]'; // matrix for moving between sum to one space to regular latent space
  matrix[P-1,P  ] Z_to_X  = Transformation[2:P,]; // matrix for moving between sum to one space to regular latent space
  vector[T-T_known] ones;
  matrix[T-T_known, P  ] add_X;
  ones = rep_vector(1,T-T_known);
  add_X = ones * Transformation[1,];
  add_X *= 1/sqrt(P);
}

parameters {
  matrix[T - T_known, P-1] z_unknown; // unknown states (proportions)
  corr_matrix[P-1] Omega_z; 
  vector<lower=0>[P-1] sigma_z; 
}

transformed parameters {
  matrix[T,P-1] z;
  matrix[P-1,P-1] Sigma_Z; 
  matrix[P,P] Sigma_X; 
  matrix[N, P] mu = rep_matrix(0, N, P);
  // states (proportions)
  matrix[T, P] x = rep_matrix(0, T, P);

  // setup x with known and unknown x
  x[x_known_t, ] = x_known;
  x[x_unknown_t, ] = z_unknown * Z_to_X;
  x[x_unknown_t, ] += add_X;
  // moving to z
  z = x * X_to_Z;
  // sum over period to handle weight periods
  for(p in 1:P){
    for(l in 1:L) {
      mu[tw_i[l], p] += tw[l] * x[tw_t[l], p];
    }
  }
  
  
  Sigma_Z = quad_form_diag(Omega_z, sigma_z); 
  
  Sigma_X = X_to_Z * Sigma_Z * X_to_Z';
  
}


model {
  // priors
  // x[1] ~ normal(0.5, 1);

  // sigma_x ~ normal(0, sigma_x_hyper);
  target += normal_lpdf(sigma_z | 0, sigma_x_hyper);
  Omega_z ~ lkj_corr(3); // LKJ prior on the correlation matrix 
  // latent state
    
    for(i in 1:(T - T_known)) {
      
        int t = x_unknown_t[i];
        if(t>1)
          target += multi_normal_lpdf(z[t,]| z[t-1, ], Sigma_Z);
        
      
    }
    for(i in 1:T_known) {
        int t = x_known_t[i];
        if(t>1)
          target += multi_normal_lpdf(z[t,]| z[t-1, ], Sigma_Z);
    }

  // Observations
  for(i in 1:N) {
      // y[i] ~ normal(mu[i], sigma_y[i]);
      target += normal_lpdf(y[i,] | mu[i,], sigma_y[i,]);
    
  }
}
generated quantities{
  matrix[N,P] y_pred;

    for(i in 1:N) {
      for(p in 1:P){
        y_pred[i,p] = normal_rng(mu[i,p], sigma_y[i,p]);
      }
    }
}


