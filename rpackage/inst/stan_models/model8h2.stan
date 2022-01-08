// Built from model 8g1
// The same model but more efficiently reparametrized
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
  int<lower=0, upper=1> use_ar_kappa;
  int<lower=0, upper=1> use_t_dist_industry_bias;

  // use a prop latent state
  int<lower=0> use_latent_state_version;

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
  real<lower=0> g_t[T]; // years since last election
  real<lower=0> g_i[N]; // g for each poll
  int<lower=1, upper=T_known + 1> next_known_state_poll_index[N]; // The index of the next known state
  int<lower=1, upper=T_known + 1> next_known_state_t_index[T]; // The index of the next known state


  // House bias and design effects
  // slower time s and house of polls
  int<lower=1, upper=S> s_i[N];
  int<lower=1, upper=H> h_i[N];

  // slower time s by time point
  int<lower=1, upper=S> s_t[T];

  // The industry bias sigma_kappa prior
  // It depends on the length between known states
  real<lower=0> sigma_beta_mu_sigma_hyper;
  real<lower=0> beta_mu_1_sigma_hyper;
  int<lower=0, upper=1> estimate_alpha_beta_mu;
  real<lower=-1,upper=1> alpha_beta_mu_known[1];

  real<lower=0> sigma_beta_sigma_sigma_hyper;
  real<lower=0> beta_sigma_1_sigma_hyper;
  int<lower=0, upper=1> estimate_alpha_beta_sigma;
  real<lower=-1,upper=1> alpha_beta_sigma_known[1];

  real<lower=0> sigma_kappa_hyper;
  real<lower=0> kappa_1_sigma_hyper;
  int<lower=0, upper=1> estimate_alpha_kappa;
  real<lower=-1,upper=1> alpha_kappa_known[1];

  // Contraint priors
  real<lower=0> kappa_sum_sigma_hyper;
  real<lower=0> beta_mu_sum_party_sigma_hyper;
  real<lower=0> beta_mu_sum_house_sigma_hyper;

  // Estimate kappa_next
  int<lower=0, upper=1> estimate_kappa_next;

  real<lower=0> g_scale;

  // nu_kappa prior
  real<lower=0> nu_kappa_raw_alpha;
  real<lower=0> nu_kappa_raw_beta;

  // nu_lkj
  real<lower=0> nu_lkj;

  // x_{t=1} prior (Dirichlet(x1_prior_p, x1_prior_alpha0))
  real<lower=0, upper = 1> x1_prior_p[P + 1];
  real<lower=0> x1_prior_alpha0;

  // use multivariate prior (0 is univariate, 1 is just one corr matrix, 2 is one per s)
  int<lower=0> use_multivariate_version;
  int<lower=0, upper=1> use_softmax; // use multivariate softmax

  // alpha priors
  real alpha_kappa_mean;
  real<lower=0> alpha_kappa_sd;
  real alpha_beta_mu_mean;
  real<lower=0> alpha_beta_mu_sd;
  real alpha_beta_sigma_mean;
  real<lower=0> alpha_beta_sigma_sd;
}

transformed data {
  // Compute hyperparameter based on time_scale_length
  real<lower=0, upper=1> sigma_x_hyper = 0.25 * sqrt(time_scale_length / 30);
  int<lower=0, upper=P> no_sigma_xc = 0;
  real<lower=0> gs_t[T];
  real<lower=0> gs_i[N];
  int Px = P;
  int no_unknown_kappa = 0;
  int use_jump_process = 0;
  int t_start_all = min(t_start); // starting point for latent state
  int t_end_all = max(t_end); // end point of latent state
  int no_Omega = 1;
  int s_t_Omega[T] = rep_array(1, T); // map between time point and corr matrix
  matrix[P,P] Omega_identity = diag_matrix(rep_vector(1.0, P));
  int use_multivariate_model = 0;
  matrix[T_known, P] eta_known = rep_matrix(0.0, T_known, P);
  real x_known_other[T_known] =  rep_array(0.0, T_known);
  row_vector[P] t1_prior_mu;
  row_vector[P] t1_prior_sigma;

  if(use_latent_state_version == 2)
    no_sigma_xc = P;
  if(use_latent_state_version == 3)
    no_sigma_xc = P;
  if(use_latent_state_version == 4)
    no_sigma_xc = 1;
  if(use_latent_state_version == 5)
    use_jump_process = 1;

  if(use_multivariate_version > 0)
    use_multivariate_model = 1;

  if(use_softmax){
    // Compute known eta
    Px = P + 1;
    // Compute final element of x_known (sum to one constraint)
    for(t in 1:T_known){
      for(p in 1:P){
        x_known_other[t] += x_known[t,p];}
      x_known_other[t] = 1 - x_known_other[t];}

    // Compute known eta and normalize with last value of x_known
    // this create a softmax known value with other being a reference at 0
    for(t in 1:T_known)
      for(p in 1:P)
        eta_known[t,p] = log(x_known[t,p]) - log(x_known_other[t]);
  }

  if(use_multivariate_version == 2){
    no_Omega = S;
    s_t_Omega = s_t;
  }

  for(t in 1:T)
    gs_t[t] = g_t[t]/g_scale;
  for(i in 1:N)
    gs_i[i] = g_i[i]/g_scale;

  if(estimate_kappa_next == 1)
    no_unknown_kappa = T_known + 1;
  else
    no_unknown_kappa = T_known;

  // Compute normal approximation of Dirichlet
  if(use_softmax){
  // x_{t=1} prior (Dirichlet(x1_prior_p, x1_prior_alpha0))
    for(p in 1:P){
      t1_prior_mu[p] = digamma(x1_prior_p[p] * x1_prior_alpha0) - digamma(x1_prior_p[Px] * x1_prior_alpha0);
      t1_prior_sigma[p] = sqrt(trigamma(x1_prior_p[p] * x1_prior_alpha0) + trigamma(x1_prior_p[Px] * x1_prior_alpha0));
    }
  } else {
    for(p in 1:P){
      t1_prior_mu[p] = x1_prior_p[p];
      t1_prior_sigma[p] = sqrt(x1_prior_p[p] * (1 - x1_prior_p[p]) / (x1_prior_alpha0 + 1));
    }
  }

}

parameters {
  matrix<lower=0, upper=1>[use_softmax ? 0 : T - T_known, use_softmax ? 0 : Px] x_unknown; // unknown states (proportions)
  matrix[use_softmax ? T - T_known : 0, use_softmax ? P : 0] eta_unknown; // unknown states (proportions)
  vector<lower=0>[P] sigma_x; // dynamic movement
  real<lower=0> sigma_xc[no_sigma_xc];
  matrix[use_industry_bias ? no_unknown_kappa : 0, use_industry_bias ? P : 0] kappa_raw; // Industry bias
  vector<lower=0>[use_industry_bias ? P : 0] sigma_kappa; // Industry bias effect
  real beta_mu[use_house_bias ? S : 0, use_house_bias ? H : 0, use_house_bias ? P : 0];
  real<lower=0> sigma_beta_mu[use_house_bias ? 1 : 0];
  real beta_sigma[use_design_effects ? S : 0, use_design_effects ? H : 0];
  real<lower=0> sigma_beta_sigma[use_design_effects ? 1 : 0];
  real<lower=-1,upper=1> alpha_kappa_unknown[estimate_alpha_kappa ? 1 : 0];
  real<lower=-1,upper=1> alpha_beta_mu_unknown[estimate_alpha_beta_mu ? 1 : 0];
  real<lower=-1,upper=1> alpha_beta_sigma_unknown[estimate_alpha_beta_sigma ? 1 : 0];
  vector<lower=0>[use_t_dist_industry_bias ? 1 : 0] nu_kappa_raw;
  vector<lower=0>[use_t_dist_industry_bias ? 1 : 0] v_kappa;
  matrix<lower=0>[use_jump_process ? T : 0, use_jump_process ? P : 0] V_noise;
  vector<lower=2,upper=4>[use_jump_process ? P : 0] alpha_V; //shape of jumps
  vector<lower=0,upper=1>[use_jump_process ? P : 0] ar_V; // AR component for jump
  vector<lower=0,upper=1>[use_jump_process ? P : 0] theta_x; // proportion of jump vs Gauss
  cholesky_factor_corr [use_multivariate_model ? P : 0] L_Omega_x[use_multivariate_model ? no_Omega : 0]; // correlation matrix
}

transformed parameters {
  matrix[N, P] mu = rep_matrix(0, N, P);
  // states (proportions)
  matrix<lower=0, upper=1>[T, Px] x = rep_matrix(0.0, T, Px);
  matrix[use_softmax ? T : 0, use_softmax ? P : 0] eta = rep_matrix(0.0, use_softmax ? T : 0, use_softmax ? P : 0);
  matrix[use_softmax ? T : 0, use_softmax ? Px : 0] eta_full = rep_matrix(0.0, use_softmax ? T : 0, use_softmax ? Px : 0);
  vector[use_constrained_party_kappa ? no_unknown_kappa : 0] kappa_sum_T_known_plus_1  = rep_vector(0, use_constrained_party_kappa ? no_unknown_kappa : 0);
  matrix[use_constrained_party_house_bias ? S : 0, use_constrained_party_house_bias ? H : 0] beta_mu_sum_H = rep_matrix(0, use_constrained_party_house_bias ? S : 0, use_constrained_party_house_bias ? H : 0);
  matrix[use_constrained_house_house_bias ? S : 0, use_constrained_house_house_bias ? P : 0] beta_mu_sum_P = rep_matrix(0, use_constrained_house_house_bias ? S : 0, use_constrained_house_house_bias ? P : 0);
  matrix[use_industry_bias ? (T_known + 1) : 0, use_industry_bias ? P : 0] kappa; // Industry bias
  real<lower=-1,upper=1> alpha_kappa[1] = alpha_kappa_known;
  real<lower=-1,upper=1> alpha_beta_mu[1] = alpha_beta_mu_known;
  real<lower=-1,upper=1> alpha_beta_sigma[1] = alpha_beta_sigma_known;
  vector<lower=1>[use_t_dist_industry_bias ? 1 : 0] nu_kappa = rep_vector(2, use_t_dist_industry_bias ? 1 : 0);
  matrix<lower=0>[use_jump_process ? T : 0, use_jump_process ? P : 0] V;
  cholesky_factor_corr[P] L_Omega[no_Omega];

  // setup x with known and unknown x
  if(use_softmax){
    // compute x based on softmax and assign known values to eta
    eta[x_known_t, ] = eta_known;
    eta[x_unknown_t, ] = eta_unknown;

    //  Tranform eta to x through softmax
    eta_full = append_col(eta, rep_matrix(0.0, T, 1));
    for(t in 1:T){
      x[t, ] = to_row_vector(softmax(to_vector(eta_full[t, ])));
    }
  } else {
    x[x_known_t, ] = x_known;
    x[x_unknown_t, ] = x_unknown;
  }

// setup multivariate omega
  if(use_multivariate_version > 0){ // multivariate
    L_Omega = L_Omega_x;
  } else { // univariate
    for(i in 1:no_Omega)
      L_Omega[i] = Omega_identity;
  }


  // setup x with known x = 0 when parties does not exist
  for(p in 1:P){
    // we need to set values 2 steps before to 0,
    // since the value before is used as a prior
    // this could be handled in the model instead
    // If set to 0, this forces the first time step to jump from 0
    if(t_start[p] > 2){ // note: the first value is the state-space prior
      for(t in 1:(t_start[p]-2))
        x[t,p] = 0.0;
    }
    if(t_end[p] < T){
      for(t in (t_end[p] + 1):T)
        x[t,p] = 0.0;
    }
  }

  // sum over period to handle weight periods
  for(p in 1:P)
    for(l in 1:L)
      mu[tw_i[l], p] += tw[l] * x[tw_t[l], p];

  // Add industry bias
  if(use_industry_bias){
    if(estimate_alpha_kappa){
      alpha_kappa = alpha_kappa_unknown;
    }
    if(use_t_dist_industry_bias){
      nu_kappa[1] = nu_kappa_raw[1] + 1.0;
    }
    for(p in 1:P){
      // non-centering
      if(use_ar_kappa){
        if(use_t_dist_industry_bias){
          kappa[1, p] = sqrt(v_kappa[1]) * kappa_raw[1, p] * kappa_1_sigma_hyper;
          for(j in 2:no_unknown_kappa)
            kappa[j, p] = alpha_kappa[1] * kappa[j-1, p] + sqrt(v_kappa[1]) * kappa_raw[j, p] * sigma_kappa[p];
        } else {
          kappa[1, p] = kappa_raw[1, p] * kappa_1_sigma_hyper;
          for(j in 2:no_unknown_kappa)
            kappa[j, p] = alpha_kappa[1] * kappa[j-1, p] + kappa_raw[j, p] * sigma_kappa[p];
        }
      } else {
        if(use_t_dist_industry_bias){
          for(j in 1:no_unknown_kappa)
             kappa[j,p] = sqrt(v_kappa[1]) * kappa_raw[j, p] * sigma_kappa[p];
        } else {
          for(j in 1:no_unknown_kappa)
             kappa[j,p] = kappa_raw[j, p] * sigma_kappa[p];
        }
      }
      if(estimate_kappa_next == 0){
          kappa[T_known + 1, p] = 0;
      }
      for(i in 1:N)
        if(y_missing[i, p] == 0)
            mu[i,p] = mu[i,p] + gs_i[i] * kappa[next_known_state_poll_index[i], p];
      }
    }

  // Add house bias
  if(use_house_bias){
    if(estimate_alpha_beta_mu)
      alpha_beta_mu = alpha_beta_mu_unknown;
    for(p in 1:P)
      for(i in 1:N)
        if(y_missing[i, p] == 0)
            mu[i,p] = mu[i,p] + beta_mu[s_i[i],h_i[i],p];
    }

  // Add design effects (mainly handled in model block)
  if(use_design_effects)
    if(estimate_alpha_beta_mu)
      alpha_beta_sigma = alpha_beta_sigma_unknown;

  // Add soft constrain over parties for Kappa
  if(use_constrained_party_kappa)
    for(t in 1:no_unknown_kappa)
      for(p in 1:P)
        kappa_sum_T_known_plus_1[t] += kappa[t,p];

  // Add soft constrain over parties for beta_mu
  if(use_constrained_party_house_bias)
    for(s in 1:S)
      for(h in 1:H)
        for(p in 1:P)
            beta_mu_sum_H[s,h] += beta_mu[s,h,p];

  // Add soft constrain over houses for beta_mu
  if(use_constrained_house_house_bias)
    for(s in 1:S)
      for(p in 1:P)
        for(h in 1:H)
          beta_mu_sum_P[s,p] += beta_mu[s,h,p];

  if(use_jump_process){
    for(p in 1:P){
      V[t_start[p]-1, p] = V_noise[t_start[p]-1, p] ;
      for(t in t_start[p]:t_end[p]) {
        V[t, p] = V_noise[t, p] + ar_V[p] * V[t-1, p];
  }}}


}


model {
  vector[P] sigma_latent;
  // Print for debugging
  // print(Omega_identity);
  // print(no_Omega);
  // print(s_t_Omega);

  // print("eta:", eta);
  // print("sigma_x:", sigma_x);


  // priors
  // x[1] ~ normal(0.5, 1);
  if(estimate_alpha_kappa)
    target += normal_lpdf(alpha_kappa_unknown | alpha_kappa_mean, alpha_kappa_sd);
  if(estimate_alpha_beta_mu)
    target += normal_lpdf(alpha_beta_mu_unknown | alpha_beta_mu_mean, alpha_beta_mu_sd);
  if(estimate_alpha_beta_sigma)
    target += normal_lpdf(alpha_beta_sigma_unknown | alpha_beta_sigma_mean, alpha_beta_sigma_sd);

  if(use_multivariate_version > 0)
    for(i in 1:no_Omega)
      target += lkj_corr_cholesky_lpdf(L_Omega[i] | nu_lkj);

  // sigma_x ~ normal(0, sigma_x_hyper);
  target += normal_lpdf(sigma_x | 0, sigma_x_hyper);

  // latent state dynamics
  sigma_latent = sigma_x;
  if(use_latent_state_version == 5){
    for(p in 1:P)
      target += inv_gamma_lpdf(V_noise[t_start_all - 1, p]| alpha_V[p], (alpha_V[p] - 1)*(1 - ar_V[p]));
  }

  if(use_softmax){
    for(p in 1:P){ // t=1 prior
      target += normal_lpdf(eta[1,p] | t1_prior_mu[p], t1_prior_sigma[p]);
    }
  } else {
    for(p in 1:P){ // t=1 prior
      target += normal_lpdf(x[1,p] | t1_prior_mu[p], t1_prior_sigma[p]);
    }
  }

  for(t in t_start_all:t_end_all){
    if(use_latent_state_version == 1) for(p in 1:P) sigma_latent[p] = x[t-1, p] * sigma_x[p] + 0.0001;
    if(use_latent_state_version == 2) for(p in 1:P) sigma_latent[p] = x[t-1, p] * sigma_x[p] + sigma_xc[p];
    if(use_latent_state_version == 3) for(p in 1:P) sigma_latent[p] = sqrt(x[t-1, p] * (1 - x[t-1, p])) * sigma_x[p] + sigma_xc[p];
    if(use_latent_state_version == 4) for(p in 1:P) sigma_latent[p] = sqrt(x[t-1, p] * (1 - x[t-1, p])) * sigma_x[p] + sigma_xc[1];
    if(use_latent_state_version == 5) {
      for(p in 1:P) {
        target += inv_gamma_lpdf(V_noise[t, p] | alpha_V[p],   (alpha_V[p] - 1) * (1 - ar_V[p]));
        sigma_latent[p] = sigma_x[p] * ((1-theta_x[p])  * V[t,p]  + theta_x[p]);
    }}
    if(use_softmax){
      // eta_z ~ std_normal();
      // print(target());
      target += multi_normal_cholesky_lpdf(eta[t, ] | eta[t-1, ], diag_pre_multiply(sigma_latent, L_Omega[s_t_Omega[t]]));
    } else {
      target += multi_normal_cholesky_lpdf(x[t, ] | x[t-1, ], diag_pre_multiply(sigma_latent, L_Omega[s_t_Omega[t]]));
    }

  }

  // Industry bias prior
  if(use_industry_bias){
    if(use_t_dist_industry_bias){
      target += gamma_lpdf(nu_kappa_raw | nu_kappa_raw_alpha, nu_kappa_raw_beta);
      target += gamma_lpdf(v_kappa | nu_kappa, nu_kappa - 1);
    }
    for(p in 1:P){
      for(j in 1:no_unknown_kappa) {
        target += std_normal_lpdf(kappa_raw[j, p]);
      }
      target += normal_lpdf(sigma_kappa[p] | 0, sigma_kappa_hyper);
    }
  }

  // Add soft constraint prior for Kappa
  if(use_constrained_party_kappa){
    for(t in 1:no_unknown_kappa){
        target += normal_lpdf(kappa_sum_T_known_plus_1[t] | 0, kappa_sum_sigma_hyper);
    }
  }
  // Add soft constrain over parties for beta_mu
  if(use_constrained_party_house_bias)
    for(s in 1:S)
      for(h in 1:H)
        target += normal_lpdf(beta_mu_sum_H[s,h] | 0, beta_mu_sum_party_sigma_hyper);

  // Add soft constrain over houses for beta_mu
  if(use_constrained_house_house_bias)
    for(s in 1:S)
      for(p in 1:P)
        target += normal_lpdf(beta_mu_sum_P[s,p] | 0, beta_mu_sum_house_sigma_hyper);

  // House bias prior
  if(use_house_bias){
    for(p in 1:P){
      for(h in 1:H) {
        target += normal_lpdf(beta_mu[1,h,p] | 0, beta_mu_1_sigma_hyper);
        if(S > 1){
          for(s in 2:S) {
            target += normal_lpdf(beta_mu[s, h, p] | alpha_beta_mu[1] * beta_mu[s - 1, h, p], sigma_beta_mu);
          }
        }
      }
    }
    target += normal_lpdf(sigma_beta_mu | 0, sigma_beta_mu_sigma_hyper);
  }

  // Design effects prior
  if(use_design_effects){
    for(h in 1:H) {
      target += normal_lpdf(beta_sigma[1,h] | 0, beta_sigma_1_sigma_hyper);
      if(S > 1){
        for(s in 2:S) {
          target += normal_lpdf(beta_sigma[s, h] | alpha_beta_sigma[1] * beta_sigma[s - 1, h], sigma_beta_sigma);
        }
      }
    }
    target += normal_lpdf(sigma_beta_sigma | 0, sigma_beta_sigma_sigma_hyper);
  }

  // Observations with and without design effects
  if(use_design_effects){
    for(p in 1:P)
      for(i in 1:N)
        if(y_missing[i, p] == 0)
          target += normal_lpdf(y[i,p] | mu[i,p], sigma_y[i,p] * exp(beta_sigma[s_i[i],h_i[i]]));
  } else {
    for(p in 1:P)
      for(i in 1:N)
        if(y_missing[i, p] == 0)
          target += normal_lpdf(y[i,p] | mu[i,p], sigma_y[i,p]);
  }

  // print("Final target:", target());
}

generated quantities{
  matrix[use_industry_bias ? (T_known + 1) : 0, use_industry_bias ? P : 0] kappa_pred;
  matrix[T, Px] x_pred; // Note that we do not limit draws with x_pred too low or too high
  real min_x_pred; // Minimal x_pred to use for rejection sampling
  int max_it = 10; // Maximum iterations to try rejection sampling
  matrix[P,P] Omega[no_Omega]; // correlation matrix

  kappa_pred = kappa;
  x_pred = x;
  // Compute the correlation matrix
  for(i in 1:no_Omega)
    Omega[i] = multiply_lower_tri_self_transpose(L_Omega[i]);

  for(p in 1:P){
    min_x_pred = -1.0; // To start the while loop
    for(i in 1:max_it) {
      if(min_x_pred > 0) {break;} // rejection sampling - minimum x_pred needs to be larger than 0
      if(estimate_kappa_next == 0){
        // Compute the predictive distribution of next kappa,
        // instead of estimating it and correct x
        if(estimate_alpha_kappa){
          if(use_t_dist_industry_bias){
            kappa_pred[(T_known + 1), p] = student_t_rng(2 * nu_kappa[1], alpha_kappa[1] * kappa[no_unknown_kappa, p], sqrt(0.5 * (nu_kappa[1] - 1) / nu_kappa[1]) * sigma_kappa[p]);
          } else {
            kappa_pred[(T_known + 1), p] = normal_rng(alpha_kappa[1] * kappa[no_unknown_kappa, p], sigma_kappa[p]);
          }
        } else {
          if(use_t_dist_industry_bias){
            kappa_pred[(T_known + 1), p] = student_t_rng(2 * nu_kappa[1], 0, sqrt(0.5 * (nu_kappa[1] - 1) / nu_kappa[1]) * sigma_kappa[p]);
          } else {
            kappa_pred[(T_known + 1), p] = normal_rng(0, sigma_kappa[p]);
          }
        }
        // estimate x_pred
        for(t in 1:T){
          if(next_known_state_t_index[t] == (T_known + 1)){
            x_pred[t,p] = x[t,p] - gs_t[t] * kappa_pred[(T_known + 1), p];
            }
          }
        min_x_pred = min(x_pred[,p]);
      } else {
        min_x_pred = 1.0; //
      }
    }
  }


}

