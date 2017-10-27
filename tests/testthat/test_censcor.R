gen_data <- function(N, rho, L_prob) {
  sigma <- matrix(
    c(1, rho, rho, 1),
    ncol = 2, byrow = T
  )
  mu <- rep(0, 2)
  
  X <- MASS::mvrnorm(n = N, mu, sigma)
  
  L <- quantile(X[,1], probs = c(L_prob))
  
  X_cens <- X
  cens <- X_cens < L
  X_cens[cens] <- L
  
  return(list(X_cens = X_cens, X = X, cens = cens, L = L))
}

test_that("censcor works", {
  set.seed(1)
  d1 <- gen_data(100, 0.5, 0.2)
  
  fit <- censcor(d1$X_cens[,1], d1$X_cens[,2], d1$cens[,1], d1$cens[,2], seed = 1)
  
  expect_equal(rstan::summary(fit)$summary["rho", "50%"], 0.397, tol = 0.01)
})
