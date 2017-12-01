// Based on Doorn et al. 2017, "Bayesian Estimation of Kendall's Tau Using a Latent Normal Approach." https://arxiv.org/pdf/1703.01805.pdf
// adapted to handle censored values
functions {
  real binormal_cdf(real z1, real z2, real rho) {
    if (z1 != 0 || z2 != 0) {
      real denom = fabs(rho) < 1.0 ? sqrt((1 + rho) * (1 - rho)) : not_a_number();
      real a1 = (z2 / z1 - rho) / denom;
      real a2 = (z1 / z2 - rho) / denom;
      real product = z1 * z2;
      real delta = product < 0 || (product == 0 && (z1 + z2) < 0);
      return 0.5 * (Phi(z1) + Phi(z2) - delta) - owens_t(z1, a1) - owens_t(z2, a2);
    }
    return 0.25 + asin(rho) / (2 * pi());
  }
}
data {
  int N;
  int x_rank[N];
  int y_rank[N];
  int x_cens[N];
  int y_cens[N];
}
parameters {
  ordered[N] x;
  ordered[N] y;
  real<lower=-1, upper=1> rho;
}
transformed parameters {
  real x2[N];
  real y2[N];
  matrix[2, 2] T;
  real sigma_adj;
  
  for(i in 1:N) {
    x2[i] = x[x_rank[i]];
    y2[i] = y[y_rank[i]];
  }
  
  T[1, 1] = 1;
  T[1, 2] = rho;
  T[2, 1] = rho;
  T[2, 2] = 1;
  
  sigma_adj = sqrt(1 - square(rho));
}
model {
  vector[2] m;
  
  for(i in 1:N) {
    if(x_cens[i] == 0 && y_cens[i] == 0) {
      m[1] = x2[i];
      m[2] = y2[i];
      target += multi_normal_lpdf(m | [0, 0], T);
    }
    else if(x_cens[i] == -1 && y_cens[i] == 0) {
      target += normal_lpdf(y2[i] | 0, 1);
      #target += normal_lcdf(0 | rho * y2[i], sigma_adj);
    }
    else if(x_cens[i] == 0 && y_cens[i] == -1) {
      target += normal_lpdf(x2[i] | 0, 1);
      #target += normal_lcdf(0 | rho * x2[i], sigma_adj);
    }
    else if(x_cens[i] == -1 && y_cens[i] == -1) {
      #target += log(binormal_cdf(x2[i], y2[i], rho));
    }
  }
}
generated quantities {
  real tau;
  tau = 2 / pi() * asin(rho);
}
