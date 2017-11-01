test_that("parse_formula works", {
  test_that("parse_formula works with no censoring", {
    res <- censcor:::parse_formula(y ~ x, data.frame(x = 1:10, y = 1:10))
    
    expect_equal(res$x, 1:10)
    expect_equal(res$y, 1:10)
    expect_null(res$cens_x)
    expect_null(res$cens_y)
    expect_null(res$cens_x_upper)
    expect_null(res$cens_y_upper)
  })
})