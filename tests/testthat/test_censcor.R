gen_data <- function(N, rho, L_prob) {
  sigma <- matrix(
    c(1, rho, rho, 1),
    ncol = 2, byrow = T
  )
  mu <- rep(0, 2)
  
  X <- MASS::mvrnorm(n = N, mu, sigma)
  
  if(L_prob == 0) {
    L <- -Inf
  }
  else {
    L <- quantile(X[,1], probs = c(L_prob))
  }
  
  X_cens <- X
  cens <- X_cens < L
  X_cens[cens] <- L
  
  df <- data.frame(
    x = X_cens[,1],
    y = X_cens[,2],
    cens_x = cens[,1],
    cens_y = cens[,2]
  )
  return(list(X_cens = X_cens, X = X, cens = cens, L = L, df = df))
}

gen_data_z <- function(N, rho, L_prob, v = 0.5) {
  sigma <- matrix(
    c(1, rho, rho, 1),
    ncol = 2, byrow = T
  )
  mu <- rep(0, 2)
  
  X <- MASS::mvrnorm(n = N, mu, sigma)
  
  z <- rnorm(N, 0, v)
  X_adj = X - z
  
  L <- quantile(X[,1], probs = c(L_prob))
  
  X_cens <- X_adj
  cens <- X_cens < L
  X_cens[cens] <- L
  
  return(list(X = X, X_adj = X_adj, X_cens = X_cens, cens = cens, z = z))
}

test_that("censcor works", {
  test_that("censcor works for singly left-censored data", {
    set.seed(1)
    d1 <- gen_data(100, 0.5, 0.2)$df
    
    fit <- censcor(x | cens(cens_x) ~ y | cens(cens_y), d1, seed = 1)
    
    expect_equal(rstan::summary(fit)$summary["rho", "50%"], 0.42, tol = 0.01)
  })
  
  test_that("censcor works for interval censored data", {
    set.seed(2)
    d1 <- gen_data(100, 0.5, 0.2)$df
    d1$x_lower = ifelse(d1$cens_x, -5, d1$x)
    d1$y_lower = ifelse(d1$cens_y, -5, d1$y)
    
    fit <- censcor(x_lower | cens(cens_x, x) ~ y_lower | cens(cens_y, y), d1, seed = 1, iter = 200)
  })
  
  #test_that("censcor works for multiply censored data arising from adjustment by a normally distributed variable", {
  #  set.seed(1)
  #  d1 <- gen_data_z(200, 0.5, 0.2, 0.5)
  #  
  #  fit <- censcor(d1$X_cens[,1], d1$X_cens[,2], d1$cens[,1], d1$cens[,2], d1$z, seed = 1)
  #  
  #  expect_equal(rstan::summary(fit)$summary["cor_xy", "50%"], 0.68, tol = 0.01)
  #})
})
