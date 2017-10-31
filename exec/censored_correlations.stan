functions {
  #include "binormal_cdf.stan"
}
data {
  int N;
  real x[N];
  real y[N];
  int cens_x[N];
  int cens_y[N];
}
parameters {
  real<lower=0> sigma_x;
  real<lower=0> sigma_y;
  
  real mu_x;
  real mu_y;
  real<lower=-1,upper=1> rho;
}
transformed parameters {
  vector[2] mu;
  cov_matrix[2] T;
  
  T[1,1] = square(sigma_x);
  T[1,2] = rho * sigma_x * sigma_y;
  T[2,1] = rho * sigma_x * sigma_y;
  T[2,2] = square(sigma_y);
  
  mu[1] = mu_x;
  mu[2] = mu_y;
}
model {
  // Priors
  mu_x ~ normal(0, 1);
  mu_y ~ normal(0, 1);
  
  sigma_x ~ normal(1, 1);
  sigma_y ~ normal(1, 1);
  
  // Likelihood
  for(i in 1:N) {
    if(cens_x[i] == 0 && cens_y[i] == 0) {
      target += multi_normal_lpdf([x[i], y[i]] | mu, T);
    }
    else if(cens_x[i] == 0 && cens_y[i] == 1) {
      target += normal_lpdf(x[i] | mu_x, sigma_x) + normal_lcdf(y[i] | mu_y + rho * (sigma_y / sigma_x) * (x[i] - mu_x), sigma_y * sqrt(1 - square(rho)));
    }
    else if(cens_x[i] == 1 && cens_y[i] == 0) {
      target += normal_lpdf(y[i] | mu_y, sigma_y) + normal_lcdf(x[i] | mu_x + rho * (sigma_x / sigma_y)  * (y[i] - mu_y), sigma_x * sqrt(1 - square(rho)));
    }
    else if(cens_x[i] == 1 && cens_y[i] == 1) {
      target += log(binormal_cdf((x[i] - mu_x) / sigma_x, (y[i] - mu_y) / sigma_y, rho));
    }
  }
}
