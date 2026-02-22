data {
  int<lower=1> N;                 // number of observations
  int<lower=1> K;                 // number of control variables
  vector[N] y;                    // outcome: pragmatic vote
  vector[N] x;                    // endogenous regressor (beta)
  vector[N] z;                    // instrument (gamma)
  matrix[N, K] controls;          // control variables

  real<lower=1e-6> prior_sd;              // prior SD for both beta and gamma
  real<lower=-0.99, upper=0.99> prior_corr; // prior correlation
}

transformed data {
  matrix[2, 2] prior_cov;

  // Build covariance matrix from SD and correlation
  prior_cov[1,1] = square(prior_sd);
  prior_cov[2,2] = square(prior_sd);
  prior_cov[1,2] = prior_corr * square(prior_sd);
  prior_cov[2,1] = prior_cov[1,2];
}

parameters {
  vector[K] alpha;                // control coefficients
  vector[2] theta;                // theta[1] = gamma (on z), theta[2] = beta (on x)
  real<lower=0> sigma;            // residual standard deviation
}

model {
  // Priors
  alpha ~ normal(0, 1);
  theta ~ multi_normal([0, 0]', prior_cov);

  // Likelihood
  y ~ normal(controls * alpha + z * theta[1] + x * theta[2], sigma);
}

generated quantities {
  real gamma = theta[1];
  real beta = theta[2];
}
