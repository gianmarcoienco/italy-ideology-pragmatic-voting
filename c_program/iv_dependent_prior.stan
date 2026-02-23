data {
  int<lower=1> N;                 
  int<lower=1> K;                
  vector[N] y;                  
  vector[N] x;                    
  vector[N] z;                  
  matrix[N, K] controls; 

  real<lower=1e-6> prior_sd;            
  real<lower=-0.99, upper=0.99> prior_corr; 
}

transformed data {
  matrix[2, 2] prior_cov;

  prior_cov[1,1] = square(prior_sd);
  prior_cov[2,2] = square(prior_sd);
  prior_cov[1,2] = prior_corr * square(prior_sd);
  prior_cov[2,1] = prior_cov[1,2];
}

parameters {
  vector[K] alpha;               
  vector[2] theta;                
  real<lower=0> sigma;           
}

model {
  alpha ~ normal(0, 1);
  theta ~ multi_normal([0, 0]', prior_cov);

  y ~ normal(controls * alpha + z * theta[1] + x * theta[2], sigma);
}

generated quantities {
  real gamma = theta[1];
  real beta = theta[2];
}
