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
  real z[N];
  int cens_x[N];
  int cens_y[N];
}
parameters {
  real mu_x;
  real mu_y;
  real mu_z;
  
  real<lower=0> sigma_x;
  real<lower=0> sigma_y;
  real<lower=0> sigma_z;
  
  real<lower=-1,upper=1> rho_xy;
  real<lower=-1,upper=1> rho_xz;
  real<lower=-1,upper=1> rho_yz;
}
transformed parameters {
  vector[2] mu_xy;
  vector[2] mu_xz;
  vector[2] mu_yz;
  cov_matrix[2] T_xy;
  cov_matrix[2] T_xz;
  cov_matrix[2] T_yz;
  real sigma_x_adj;
  real sigma_y_adj;
  real rho_xy_coef;
  real rho_yx_coef;
  
  T_xy[1,1] = square(sigma_x);
  T_xy[1,2] = rho_xy * sigma_x * sigma_y;
  T_xy[2,1] = rho_xy * sigma_x * sigma_y;
  T_xy[2,2] = square(sigma_y);
  
  T_xz[1,1] = square(sigma_x);
  T_xz[1,2] = rho_xz * sigma_x * sigma_z;
  T_xz[2,1] = rho_xz * sigma_x * sigma_z;
  T_xz[2,2] = square(sigma_z);
   
  T_yz[1,1] = square(sigma_y);
  T_yz[1,2] = rho_yz * sigma_y * sigma_z;
  T_yz[2,1] = rho_yz * sigma_y * sigma_z;
  T_yz[2,2] = square(sigma_z);
  
  mu_xy[1] = mu_x;
  mu_xy[2] = mu_y;
  
  mu_xz[1] = mu_x;
  mu_xz[2] = mu_z;
  
  mu_yz[1] = mu_y;
  mu_yz[2] = mu_z;
  
  sigma_x_adj = sigma_x * sqrt(1 - square(rho_xy));
  sigma_y_adj = sigma_y * sqrt(1 - square(rho_xy));
  
  rho_xy_coef = rho_xy * (sigma_x / sigma_y);
  rho_yx_coef = rho_xy * (sigma_y / sigma_x);
}
model {
  mu_x ~ normal(0, 1);
  mu_y ~ normal(0, 1);
  mu_z ~ normal(0, 1);
  
  sigma_x ~ normal(1, 1);
  sigma_y ~ normal(1, 1);
  sigma_z ~ normal(1, 1);
  
  for(i in 1:N) {
    //
    // X, Z
    //
    
    // x and y known
    if(cens_x[i] == 0 && cens_y[i] == 0) {
      target += multi_normal_lpdf([x_lower[i], y_lower[i]] | mu_xy, T_xy);
    }
    
    // x known, y censored
    else if(cens_x[i] == 0 && cens_y[i] != 0) {
      target +=  normal_lpdf(x_lower[i] | mu_x, sigma_x);
      
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
      target += log(binormal_cdf((x_lower[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho_xy));
    }
    // Both right censored
    else if(cens_x[i] == 1 && cens_y[i] == 1) {
      // Same as both left censored if you mirror the x and y coordinates
      target += log(binormal_ccdf((x_lower[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho_xy));
    }
    // Left and right censored
    else if(cens_x[i] == -1 && cens_y[i] == 1) {
      target += log(binormal_cdf((x_lower[i] - mu_x) / sigma_x, (-y_lower[i] - mu_y) / sigma_y, -rho_xy));
    }
    // Right and left censored
    else if(cens_x[i] == 1 && cens_y[i] == -1) {
      // same as left and right censored as long as you mirror the x coordinate
      target += log(binormal_cdf((-x_lower[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, -rho_xy));
    }
    // Left and interval censored
    else if(cens_x[i] == -1 && cens_y[i] == 2) {
      target += log(binormal_cdf((x_lower[i] - mu_x) / sigma_x, (y_upper[i] - mu_y) / sigma_y, rho_xy) - binormal_cdf((x_lower[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho_xy)); 
    }
    // Right and interval censored
    else if(cens_x[i] == 1 && cens_y[i] == 2) {
      // Same as left and interval censored as long as you mirror the x coordinate
      target += log(binormal_cdf((-x_lower[i] - mu_x) / sigma_x, (y_upper[i] - mu_y) / sigma_y, rho_xy) - binormal_cdf((-x_lower[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho_xy));
    }
    // Interval and left censored
    else if(cens_x[i] == 2 && cens_y[i] == -1) {
      target += log(binormal_cdf((x_upper[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho_xy) - binormal_cdf((x_lower[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho_xy));
    }
    // Interval and right censored
    else if(cens_x[i] == 2 && cens_y[i] == 1) {
      // same as interval and left censored as long as you mirror the y coordinate
      target += log(binormal_cdf((x_upper[i] - mu_x) / sigma_x, (-y_lower[i] - mu_y) / sigma_y, rho_xy) - binormal_cdf((x_lower[i] - mu_x) / sigma_x, (-y_lower[i] - mu_y) / sigma_y, rho_xy));
    }
    // Both interval censored
    else if(cens_x[i] == 2 && cens_y[i] == 2) {
      target += log(
        // Upper right
          binormal_cdf((x_upper[i] - mu_x) / sigma_x, (y_upper[i] - mu_y) / sigma_y, rho_xy)
        
        // left
        - binormal_cdf((x_lower[i] - mu_x) / sigma_x, (y_upper[i] - mu_y) / sigma_y, rho_xy)
        
        // bottom
        - binormal_cdf((x_upper[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho_xy)
        
        // Bottom left corner, which has been subtracted twice
        + binormal_cdf((x_lower[i] - mu_x) / sigma_x, (y_lower[i] - mu_y) / sigma_y, rho_xy)
      );
    }
    
    
    //
    // X, Z
    //
    
    // X not censored
    if(cens_x[i] == 0) {
      target += multi_normal_lpdf([x_lower[i], z[i]] | mu_xz, T_xz);
    }
    // X left censored
    else if(cens_x[i] == -1) {
      target += normal_lpdf(z[i] | mu_z, sigma_z) + normal_lcdf(x_lower[i] | mu_x + rho_xz * (sigma_x / sigma_z)  * (z[i] - mu_z), sigma_x * sqrt(1 - square(rho_xz)));
    }
    // X right censored
    else if(cens_x[i] == 1) {
      target += normal_lpdf(z[i] | mu_z, sigma_z) + normal_lccdf(x_lower[i] | mu_x + rho_xz * (sigma_x / sigma_z)  * (z[i] - mu_z), sigma_x * sqrt(1 - square(rho_xz)));
    }
    // X interval censored
    else if(cens_x[i] == 2) {
      target += normal_lpdf(z[i] | mu_z, sigma_z) + log_sub_exp(
        normal_lcdf(x_upper[i] | mu_x + rho_xz * (sigma_x / sigma_z)  * (z[i] - mu_z), sigma_x * sqrt(1 - square(rho_xz))),
        normal_lcdf(x_lower[i] | mu_x + rho_xz * (sigma_x / sigma_z)  * (z[i] - mu_z), sigma_x * sqrt(1 - square(rho_xz)))
      );
    }
    
    //
    // Y, Z
    //
    
    // Y not censored
    if(cens_y[i] == 0) {
      target += multi_normal_lpdf([y_lower[i], z[i]] | mu_yz, T_yz);
    }
    // Y left censored
    else if(cens_y[i] == -1) {
      target += normal_lpdf(z[i] | mu_z, sigma_z) + normal_lcdf(y_lower[i] | mu_y + rho_yz * (sigma_y / sigma_z)  * (z[i] - mu_z), sigma_y * sqrt(1 - square(rho_yz)));
    }
    // Y right censored
    else if(cens_y[i] == 1) {
      target += normal_lpdf(z[i] | mu_z, sigma_z) + normal_lccdf(y_lower[i] | mu_y + rho_yz * (sigma_y / sigma_z)  * (z[i] - mu_z), sigma_y * sqrt(1 - square(rho_yz)));
    }
    // Y interval censored
    else if(cens_y[i] == 2) {
      target += normal_lpdf(z[i] | mu_z, sigma_z) + log_sub_exp(
        normal_lcdf(y_upper[i] | mu_x + rho_xz * (sigma_x / sigma_z)  * (z[i] - mu_z), sigma_x * sqrt(1 - square(rho_xz))),
        normal_lcdf(y_lower[i] | mu_x + rho_xz * (sigma_x / sigma_z)  * (z[i] - mu_z), sigma_x * sqrt(1 - square(rho_xz)))
      );
    }
  }
}
generated quantities {
  real sigma_xy = T_xy[1, 2];
  real sigma_xz = T_xz[1, 2];
  real sigma_yz = T_yz[1, 2];
  
  real rho_adj = (sigma_xy - sigma_xz - sigma_yz + pow(sigma_z, 2)) / sqrt((pow(sigma_x, 2) + pow(sigma_z, 2) - 2 * sigma_xz) * (pow(sigma_y, 2) + pow(sigma_z, 2) - 2 * sigma_yz));
}
