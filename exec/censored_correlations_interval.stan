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
  
  real sigma_x_adj;
  real sigma_y_adj;
  
  real rho_xy_coef;
  real rho_yx_coef;
  
  T[1,1] = square(sigma_x);
  T[1,2] = rho * sigma_x * sigma_y;
  T[2,1] = rho * sigma_x * sigma_y;
  T[2,2] = square(sigma_y);
  
  mu[1] = mu_x;
  mu[2] = mu_y;
  
  sigma_x_adj = sigma_x * sqrt(1 - square(rho));
  sigma_y_adj = sigma_y * sqrt(1 - square(rho));
  
  rho_xy_coef = rho * (sigma_x / sigma_y);
  rho_yx_coef = rho * (sigma_y / sigma_x);
}
model {
  // Priors
  mu_x ~ normal(0, 1);
  mu_y ~ normal(0, 1);
  
  sigma_x ~ normal(1, 1);
  sigma_y ~ normal(1, 1);
  
  // Likelihood
  for(i in 1:N) {
    // x and y known
    if(cens_x[i] == 0 && cens_y[i] == 0) {
      target += multi_normal_lpdf([x_lower[i], y_lower[i]] | mu, T);
    }
    
    // x known, y censored
    else if(cens_x[i] == 0 && cens_y[i] != 0) {
      target += normal_lpdf(x_lower[i] | mu_x, sigma_x);
      
      if(cens_y[i] == -1) {
        target +=  normal_lcdf(y_lower[i] | mu_y + rho_yx_coef * (x_lower[i] - mu_x), sigma_y_adj);
      }
      else if(cens_y[i] == 1) {
        target += normal_lccdf(y_lower[i] | mu_y + rho_yx_coef * (x_lower[i] - mu_x), sigma_y_adj);
      }
      else if(cens_y[i] == 2) {
        target += log_sub_exp(
            normal_lcdf(y_upper[i] | mu_y + rho_yx_coef * (x_lower[i] - mu_x), sigma_y_adj),
            normal_lcdf(y_lower[i] | mu_y + rho_yx_coef * (x_lower[i] - mu_x), sigma_y_adj)
        );
      }
    }
    
    // x censored, y known
    else if(cens_x[i] != 0 && cens_y[i] == 0) {
      target += normal_lpdf(y_lower[i] | mu_y, sigma_y);
      
      if(cens_x[i] == -1) {
        target +=  normal_lcdf(x_lower[i] | mu_x + rho_xy_coef * (y_lower[i] - mu_y), sigma_x_adj);
      }
      else if(cens_x[i] == 1) {
        target += normal_lccdf(x_lower[i] | mu_x + rho_xy_coef * (y_lower[i] - mu_y), sigma_x_adj);
      }
      else if(cens_x[i] == 2) {
        target += log_sub_exp(
            normal_lcdf(x_upper[i] | mu_x + rho_xy_coef * (y_lower[i] - mu_y), sigma_x_adj),
            normal_lcdf(x_lower[i] | mu_x + rho_xy_coef * (y_lower[i] - mu_y), sigma_x_adj)
        );
      }
    }
    
    // Both left censored
    if(cens_x[i] == -1 && cens_y[i] == -1) {
      target += log(binormal_cdf((x_lower[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho));
    }
    // Both right censored
    else if(cens_x[i] == 1 && cens_y[i] == 1) {
      // Same as both left censored if you mirror the x and y coordinates
      target += log(binormal_ccdf((x_lower[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho));
    }
    // Left and right censored
    else if(cens_x[i] == -1 && cens_y[i] == 1) {
      target += log(binormal_cdf((x_lower[i] - mu_x) / sigma_x, (-y_lower[i] - mu_y) / sigma_y, -rho));
    }
    // Right and left censored
    else if(cens_x[i] == 1 && cens_y[i] == -1) {
      // same as left and right censored as long as you mirror the x coordinate
      target += log(binormal_cdf((-x_lower[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, -rho));
    }
    // Left and interval censored
    else if(cens_x[i] == -1 && cens_y[i] == 2) {
      target += log(binormal_cdf((x_lower[i] - mu_x) / sigma_x, (y_upper[i] - mu_y) / sigma_y, rho) - binormal_cdf((x_lower[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho)); 
    }
    // Right and interval censored
    else if(cens_x[i] == 1 && cens_y[i] == 2) {
      // Same as left and interval censored as long as you mirror the x coordinate
      target += log(binormal_cdf((-x_lower[i] - mu_x) / sigma_x, (y_upper[i] - mu_y) / sigma_y, rho) - binormal_cdf((-x_lower[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho));
    }
    // Interval and left censored
    else if(cens_x[i] == 2 && cens_y[i] == -1) {
      target += log(binormal_cdf((x_upper[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho) - binormal_cdf((x_lower[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho));
    }
    // Interval and right censored
    else if(cens_x[i] == 2 && cens_y[i] == 1) {
      // same as interval and left censored as long as you mirror the y coordinate
      target += log(binormal_cdf((x_upper[i] - mu_x) / sigma_x, (-y_lower[i] - mu_y) / sigma_y, rho) - binormal_cdf((x_lower[i] - mu_x) / sigma_x, (-y_lower[i] - mu_y) / sigma_y, rho));
    }
    // Both interval censored
    else if(cens_x[i] == 2 && cens_y[i] == 2) {
      target += log(
        // Upper right
          binormal_cdf((x_upper[i] - mu_x) / sigma_x, (y_upper[i] - mu_y) / sigma_y, rho)
        
        // left
        - binormal_cdf((x_lower[i] - mu_x) / sigma_x, (y_upper[i] - mu_y) / sigma_y, rho)
        
        // bottom
        - binormal_cdf((x_upper[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho)
        
        // Bottom left corner, which has been subtracted twice
        + binormal_cdf((x_lower[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho)
      );
    }
  }
}
