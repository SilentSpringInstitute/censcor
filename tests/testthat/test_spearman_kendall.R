test_that("kendall's tau and spearman correlation estimation returns accurate values", {
  skip_on_cran()
  
  test_that("for noncensored data", {
    set.seed(1)
    d1 <- generate_censored_data(100, 0.5, 0, direction = -1)
    fit <- censcor(x ~ y, d1, seed = 1, iter = 600, method = "kendall")
    expect_equal(rstan::summary(fit)$summary["rho", "50%"], 0.435, tol = 0.001)
    expect_equal(rstan::summary(fit)$summary["tau", "50%"], 0.287, tol = 0.001)
    
    set.seed(10)
    d1 <- generate_censored_data(100, -0.75, 0, direction = -1)
    fit <- censcor(x ~ y, d1, seed = 1, iter = 600, method = "kendall")
    expect_equal(rstan::summary(fit)$summary["rho", "50%"], -0.73, tol = 0.001)
    expect_equal(rstan::summary(fit)$summary["tau", "50%"], -0.52, tol = 0.01)
  })
})
