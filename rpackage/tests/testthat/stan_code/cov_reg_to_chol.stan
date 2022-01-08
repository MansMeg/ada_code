//
// This Stan program define the function cov_reg_to_chol
// and test code for the given function

functions {
  /**
   * Return the unit diagnonal cholesky factor of a covariance matrix
   * according to Pourahmadi (2011). That is L in Eq. (14) of Pourahmadi (2011).
   *
   * @reference
   * Pourahmadi (2011) Covariance Estimation: The GLM and regularization
   * perspectives, Statistical Science.
   *
   * @param P The dimension of L
   * @param phi Regression coefficient in Eq. (16) of Pourahmadi (2011) row-wise
   * of Eq. (18) (i.e phi[1] is phi_{2,1}, phi[2] is phi_{3,1}, and
   * phi[3] is phi_{3,2}).
   *
   * @return A cholesky factor L with unit diagonal.
   */
  matrix cov_reg_to_chol(int P, vector phi) {
    matrix[P,P] T; // This is actually a cholesky for a precision matrix
    matrix[P,P] L;
    {
    int counter = 1;
      T[1,1]=1;
      for(i in 2:P){
        T[i, i]=1;
        for(j in 1:(i-1)){
          T[i, j] = phi[counter];
          T[j, i] = 0;
          counter += 1;
        }
      }
    }
    L = inverse(T);
    // L = chol2inv(T);
    for(i in 2:P){
      for(j in 1:(i-1)){
        L[j, i] = 0;
      }
    }
    return L;
  }
}

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> N; // number of observations
  int<lower=2> P; // dimension
  matrix[N,P]  Y; // data
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[P*(P-1)/2] beta;
  vector<lower=0>[P] sigma;
  vector[P] mu;
}



transformed parameters {
//  matrix[P, P] T = rep_matrix(0, P, P);
//  cholesky_factor_cov[P] T; // This is actually a cholesky for a precision matrix
  cholesky_factor_cov[P] L;
  matrix[P, P]  Q;
  L = cov_reg_to_chol(P, beta);
  Q = crossprod(diag_pre_multiply(sigma,L));
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {

  for(n in 1:N){
     Y[n,] ~ multi_normal(mu, Q);
  }

}

