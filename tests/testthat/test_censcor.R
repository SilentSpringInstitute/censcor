

gen_censored_data_z <- function(N, rho, L_prob, v = 0.5) {
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
  test_that("censcor works for noncensored data", {
    set.seed(1)
    d1 <- generate_censored_data(100, 0.5, 0, direction = -1)
    
    fit <- censcor(x ~ y, d1, seed = 1, iter = 600)
    
    expect_equal(rstan::summary(fit)$summary["rho", "50%"], 0.529, tol = 0.001)
  })
  
  test_that("censcor works for singly left-censored data", {
    set.seed(1)
    d1 <- generate_censored_data(100, 0.5, 0.1, direction = -1)
    
    fit <- censcor(x | cens(x_cens) ~ y | cens(y_cens), d1, seed = 1, iter = 600)
    
    expect_equal(rstan::summary(fit)$summary["rho", "50%"], 0.44, tol = 0.01)
  })
  
  test_that("censcor works for singly right-censored data", {
    set.seed(2)
    d1 <- generate_censored_data(100, 0.5, 0.1, direction = 1)
    d1$x_cens <- ifelse(d1$x_cens, 1, 0)
    d1$y_cens <- ifelse(d1$y_cens, 1, 0)
    
    fit <- censcor(x | cens(x_cens) ~ y | cens(y_cens), d1, seed = 1, iter = 600)
    
    expect_equal(rstan::summary(fit)$summary["rho", "50%"], 0.61, tol = 0.01)
  })
  
  test_that("censcor works for interval censored data", {
    set.seed(2)
    d1 <- generate_censored_data(100, 0.5, 0.1, direction = 2)
    
    fit <- censcor(x | cens(x_cens, x_upper) ~ y | cens(y_cens, y_upper), d1, seed = 1, iter = 600)
    
    expect_equal(rstan::summary(fit)$summary["rho", "50%"], 0.61, tol = 0.01)
  })
  
  test_that("censcor works for combined left- and right-censored data", {
    set.seed(2)
    d1 <- generate_censored_data(100, 0.5, 0.1, direction = c(-1, 1))
    
    fit <- censcor(x | cens(x_cens) ~ y | cens(y_cens), d1, seed = 1, iter = 600)
    
    expect_equal(rstan::summary(fit)$summary["rho", "50%"], 0.61, tol = 0.01)
  })
  
  test_that("censcor works for combined left-, right-, and interval-censored data", {
    d1 <- generate_censored_data_mixed(100, 0.5, 0.1, direction = c(-1, 1, 2))
    
    fit <- censcor(x | cens(x_cens, x_upper) ~ y | cens(y_cens, y_upper), d1, seed = 1, iter = 600)
    
    expect_equal(rstan::summary(fit)$summary["rho", "50%"], 0.52, tol = 0.01)
  })
  
  #test_that("censcor works for multiply censored data arising from adjustment by a normally distributed variable", {
  #  set.seed(1)
  #  d1 <- gen_censored_data_z(200, 0.5, 0.2, 0.5)
  #  
  #  fit <- censcor(d1$X_cens[,1], d1$X_cens[,2], d1$cens[,1], d1$cens[,2], d1$z, seed = 1)
  #  
  #  expect_equal(rstan::summary(fit)$summary["cor_xy", "50%"], 0.68, tol = 0.01)
  #})
})
