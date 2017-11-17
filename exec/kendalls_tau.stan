// Based on Doorn et al. 2017, "Bayesian Estimation of Kendall's Tau Using a Latent Normal Approach." https://arxiv.org/pdf/1703.01805.pdf
data {
  int N;
  int x_rank[N];
  int y_rank[N];
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
  
  for(i in 1:N) {
    x2[i] = x[x_rank[i]];
    y2[i] = y[y_rank[i]];
  }
  
  T[1, 1] = 1;
  T[1, 2] = rho;
  T[2, 1] = rho;
  T[2, 2] = 1;
}
model {
  vector[2] m;
  
  for(i in 1:N) {
    m[1] = x2[i];
    m[2] = y2[i];
    target += multi_normal_lpdf(m | [0, 0], T);
  }
}
generated quantities {
  real tau;
  tau = 2 / pi() * asin(rho);
}
