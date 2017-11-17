

test_that("pearson correlation estimation returns accurate values", {
  skip_on_cran()
  
  test_that("for noncensored data", {
    set.seed(1)
    d1 <- generate_censored_data(100, 0.5, 0, direction = -1)
    
    fit <- censcor(x ~ y, d1, seed = 1, iter = 600)
    
    expect_equal(rstan::summary(fit)$summary["rho", "50%"], 0.444, tol = 0.001)
  })
  
  test_that("for censored data", {
    test_that("for singly left-censored data", {
      set.seed(1)
      d1 <- generate_censored_data(100, 0.5, 0.1, direction = -1)
      
      fit <- censcor(x | cens(x_cens) ~ y | cens(y_cens), d1, seed = 1, iter = 600)
      
      expect_equal(rstan::summary(fit)$summary["rho", "50%"], 0.44, tol = 0.01)
    })
    
    test_that("for singly right-censored data", {
      set.seed(2)
      d1 <- generate_censored_data(100, 0.5, 0.1, direction = 1)
      d1$x_cens <- ifelse(d1$x_cens, 1, 0)
      d1$y_cens <- ifelse(d1$y_cens, 1, 0)
      
      fit <- censcor(x | cens(x_cens) ~ y | cens(y_cens), d1, seed = 1, iter = 600)
      
      expect_equal(rstan::summary(fit)$summary["rho", "50%"], 0.61, tol = 0.01)
    })
    
    test_that("for interval censored data", {
      set.seed(2)
      d1 <- generate_censored_data(100, 0.5, 0.1, direction = 2)
      
      fit <- censcor(x | cens(x_cens, x_upper) ~ y | cens(y_cens, y_upper), d1, seed = 1, iter = 600)
      
      expect_equal(rstan::summary(fit)$summary["rho", "50%"], 0.61, tol = 0.01)
    })
    
    test_that("for combined left- and right-censored data", {
      set.seed(2)
      d1 <- generate_censored_data(100, 0.5, 0.1, direction = c(-1, 1))
      
      fit <- censcor(x | cens(x_cens) ~ y | cens(y_cens), d1, seed = 1, iter = 600)
      
      expect_equal(rstan::summary(fit)$summary["rho", "50%"], 0.61, tol = 0.01)
    })
    
    test_that("for combined left-, right-, and interval-censored data", {
      d1 <- generate_censored_data(100, 0.5, 0.1, direction = c(-1, 1, 2))
      
      fit <- censcor(x | cens(x_cens, x_upper) ~ y | cens(y_cens, y_upper), d1, seed = 1, iter = 600)
      
      expect_equal(rstan::summary(fit)$summary["rho", "50%"], 0.52, tol = 0.01)
    })
  })
  
  test_that("for multiply censored data arising from adjustment by a normally distributed variable", {
    test_that("both variables are left censored, with adjustment", {
      set.seed(4)
      d1 <- generate_censored_data(100, rho = 0.5, L_prob = 0.2, direction = -1, adj = TRUE, adj_sigma = 1)
      
      fit <- censcor(x | cens(x_cens) ~ y | cens(y_cens), d1, adj = ~adj, seed = 1, iter = 600)
      
      expect_equal(rstan::summary(fit)$summary["rho_adj", "50%"], 0.709, tol = 0.01)
    })
  })
})
