#' Estimate correlation between two censored variables
#' 
#' @export
censcor <- function(x, y, cens_x, cens_y, ...) {
  same_length = (length(x) == length(y)) && (length(x) == length(cens_x)) && (length(y) == length(cens_y))
  if (!same_length) {
    stop("x, y, cens_x, and cens_y must have the same length")
  }
  
  data = list(
    N = length(x),
    x = x,
    y = y,
    cens_x = cens_x,
    cens_y = cens_y
  )
  
  sampling(stanmodels$censored_correlations, data = data, control = list(adapt_delta = 0.95, max_treedepth = 10), ...)
}
