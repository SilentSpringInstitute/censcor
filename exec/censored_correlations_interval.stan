functions {
  #include "binormal_cdf.stan"
  #include "log_sub_exp.stan"
}
data {
  int N;
  
  real x_lower[N];
  real x_upper[N];
  
  real y_lower[N];
  real y_upper[N];
  
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
      target += multi_normal_lpdf([x_lower[i], y_lower[i]] | mu, T);
    }
    else if(cens_x[i] == 0 && cens_y[i] == 1) {
      target += normal_lpdf(x_lower[i] | mu_x, sigma_x)
        + log_sub_exp(
          normal_lcdf(y_upper[i] | mu_y + rho * (sigma_y / sigma_x) * (x_lower[i] - mu_x), sigma_y * sqrt(1 - square(rho))),
          normal_lcdf(y_lower[i] | mu_y + rho * (sigma_y / sigma_x) * (x_lower[i] - mu_x), sigma_y * sqrt(1 - square(rho)))
        )
      );
    }
    else if(cens_x[i] == 1 && cens_y[i] == 0) {
      target += (
        normal_lpdf(y_lower[i] | mu_y, sigma_y)
        + log_sub_exp(
          normal_lcdf(x_upper[i] | mu_x + rho * (sigma_x / sigma_y)  * (y_lower[i] - mu_y), sigma_x * sqrt(1 - square(rho))),
          normal_lcdf(x_lower[i] | mu_x + rho * (sigma_x / sigma_y)  * (y_lower[i] - mu_y), sigma_x * sqrt(1 - square(rho)))
        )
      );
    }
    else if(cens_x[i] == 1 && cens_y[i] == 1) {
      target += log(
        # Upper right
          binormal_cdf((x_upper[i] - mu_x) / sigma_x, (y_upper[i] - mu_y) / sigma_y, rho)
        
        # left
        - binormal_cdf((x_lower[i] - mu_x) / sigma_x, (y_upper[i] - mu_y) / sigma_y, rho)
        
        # bottom
        - binormal_cdf((x_upper[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho)
        
        # Bottom left corner, which has been subtracted twice
        + binormal_cdf((x_lower[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho)
      );
    }
  }
}
