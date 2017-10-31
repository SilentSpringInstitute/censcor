#' Estimate correlation between two censored variables
#' 
#' @export
censcor <- function(formula, df, ...) {
  d <- parse_formula(formula, df)
  
  if(!is.null(d$cens_x_upper) && !is.null(d$cens_y_upper)) {
    data = list(
      N       = length(d$x),
      x_lower = d$x,
      x_upper = d$cens_x_upper,
      y_lower = d$y,
      y_upper = d$cens_y_upper,
      cens_x  = d$cens_x,
      cens_y  = d$cens_y
    ) 
    
    sampling(stanmodels$censored_correlations_interval, data = data, control = list(adapt_delta = 0.95, max_treedepth = 15), ...)
  }
  else {
    data = list(
      N      = length(d$x),
      x      = d$x,
      y      = d$y,
      cens_x = d$cens_x,
      cens_y = d$cens_y
    )
    
    sampling(stanmodels$censored_correlations, data = data, control = list(adapt_delta = 0.95, max_treedepth = 10), ...)
  }
  #else {
  #  data = list(
  #    N = length(x),
  #    x = x,
  #    y = y,
  #    z = z,
  #    cens_x = cens_x,
  #    cens_y = cens_y
  #  )
  #  
  #  sampling(stanmodels$censored_correlations_z, data = data, control = list(adapt_delta = 0.95, max_treedepth = 10))
  #}
}
