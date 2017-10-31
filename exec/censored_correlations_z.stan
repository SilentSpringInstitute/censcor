functions {
  #include "binormal_cdf.stan"
}
data {
  int N;
  real x[N];
  real y[N];
  real z[N];
  int cens_x[N];
  int cens_y[N];
}
parameters {
  real<lower=0> sigma_x;
  real<lower=0> sigma_y;
  real<lower=0> sigma_z;
  
  real mu_x;
  real mu_y;
  real mu_z;
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
}
model {
  mu_x ~ normal(0, 1);
  mu_y ~ normal(0, 1);
  mu_z ~ normal(0, 1);
  
  sigma_x ~ normal(1, 1);
  sigma_y ~ normal(1, 1);
  sigma_z ~ normal(1, 1);
  
  for(i in 1:N) {
    // X, Y
    if(cens_x[i] == 0 && cens_y[i] == 0) {
      target += multi_normal_lpdf([x[i], y[i]] | mu_xy, T_xy);
    }
    else if(cens_x[i] == 0 && cens_y[i] == 1) {
      target += normal_lpdf(x[i] | mu_x, sigma_x) + normal_lcdf(y[i] | mu_y + rho_xy * (sigma_y / sigma_x) * (x[i] - mu_x), sigma_y * sqrt(1 - square(rho_xy)));
    }
    else if(cens_x[i] == 1 && cens_y[i] == 0) {
      target += normal_lpdf(y[i] | mu_y, sigma_y) + normal_lcdf(x[i] | mu_x + rho_xy * (sigma_x / sigma_y)  * (y[i] - mu_y), sigma_x * sqrt(1 - square(rho_xy)));
    }
    else if(cens_x[i] == 1 && cens_y[i] == 1) {
      target += log(binormal_cdf((x[i] - mu_x) / sigma_x, (y[i] - mu_y) / sigma_y, rho_xy));
    }
    
    // X, Z
    if(cens_x[i] == 0) {
      target += multi_normal_lpdf([x[i], z[i]] | mu_xz, T_xz);
    }
    else if(cens_x[i] == 1) {
      target += normal_lpdf(z[i] | mu_z, sigma_z) + normal_lcdf(x[i] | mu_x + rho_xz * (sigma_x / sigma_z)  * (z[i] - mu_z), sigma_x * sqrt(1 - square(rho_xz)));
    }
    
    // Y, Z
    if(cens_y[i] == 0) {
      target += multi_normal_lpdf([y[i], z[i]] | mu_yz, T_yz);
    }
    else if(cens_y[i] == 1) {
      target += normal_lpdf(z[i] | mu_z, sigma_z) + normal_lcdf(y[i] | mu_y + rho_yz * (sigma_y / sigma_z)  * (z[i] - mu_z), sigma_y * sqrt(1 - square(rho_yz)));
    }
  }
}
generated quantities {
  real sigma_xy = T_xy[1, 2];
  real sigma_xz = T_xz[1, 2];
  real sigma_yz = T_yz[1, 2];
  
  real cor_xy = (sigma_xy - sigma_xz - sigma_yz + pow(sigma_z, 2)) / sqrt((pow(sigma_x, 2) + pow(sigma_z, 2) - 2 * sigma_xz) * (pow(sigma_y, 2) + pow(sigma_z, 2) - 2 * sigma_yz));
}
