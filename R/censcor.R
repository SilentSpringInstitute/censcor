#' Estimate correlation between two censored variables.
#' 
#' @param formula formula describing the correlation to estimate
#' @param df data frame containing the data described by the formula
#' @param method a character string indicating which correlation coefficient to compute. One of "pearson" (default), "spearman", or "kendall".
#' @param adj formula adjustment column used for data
#' @param chains number of MCMC chains to run
#' @param ... arguments passed to rstan::sampling
#' 
#' @details 
#' The most basic formula describes two variables that aren't censored:
#' 
#' x ~ y
#' 
#' where x and y are columns of the input dataframe.
#' 
#' Censoring is described through additional columns
#' 
#' x | cens(x_cens) ~ y | cens(y_cens)
#' 
#' x_cens and y_cens can contain -1, 0, 1, or 2. Alternatively, you can use the strings "left", "none", "right", and "interval".
#' "left" (-1) indicates left censoring, "none" (0) no censoring, "right" (1) right censoring, and "interval" (2) interval censoring.
#' Censored values of x and y should be replaced with the censoring limit.
#' 
#' In the case of interval censoring, there are two limits: the upper and lower censoring limits. In this case, x should contain the lower limit,
#' and the upper limit should be in a separate column:
#' 
#' x | cens(x_cens, x_upper_limit) ~ y | cens(y_cens, y_upper_limit)
#' 
#' Different censoring types can be combined in the same data frame. As before, x and y should contain the censoring limits for either left or right censoring,
#' and for interval censoring use a separate column to input the upper bounds. If you mix interval censoring with left or right censoring,
#' you can use any value in the upper limit column for non-interval censored values, because the algorithm only uses the upper limit column
#' for interval censored values.
#' 
#' `censcor` returns a `stanfit` object. The fit includes:
#' - mu_x, the estimated mean of the x variable
#' - sigma_x, the estimated standard deviation of the x variable
#' - mu_y, the estimated mean of the y variable
#' - sigma_y, the estimated standard deviation of the y variable
#' - rho, the estimated correlation between x and y
#' 
#' @return stanfit object
#' 
#' @seealso generate_censored_data
#' 
#' @references
#' Newton, Elizabeth, and Ruthann Rudel. "Estimating correlation with multiply censored data arising from the adjustment of singly censored data." Environmental science & technology 41.1 (2007): 221-228.
#' 
#' van Doorn, Johnny, et al. "Bayesian Estimation of Kendall's tau Using a Latent Normal Approach." arXiv preprint arXiv:1703.01805 (2017).
#' 
#' @examples 
#' \dontrun{
#'   # neither x or y censored
#'   df <- generate_censored_data(N = 100, rho = 0.5, L_prob = 0)
#'   censcor(x ~ y, df)
#'   
#'   # x and y are both left censored
#'   df <- generate_censored_data(N = 100, rho = 0.5, L_prob = 0.25, direction = -1)
#'   censcor(x | cens(x_cens) ~ y | cens(y_cens), df)
#'   
#'   # x and y are right and interval censored
#'   df <- generate_censored_data(N = 100, rho = 0.5, L_prob = 0.25, direction = c(1, 2))
#'   censcor(x | cens(x_cens, x_upper) ~ y | cens(y_cens, y_upper), df)
#'   
#'   # You can examine the results using functions from rstan:
#'   library(rstan)
#'   fit <- censcor(x | cens(x_cens, x_upper) ~ y | cens(y_cens, y_upper), df)
#'   summary(fit)
#' }
#' 
#' @importFrom stringr str_replace
#' @export
censcor <- function(formula, df, method = "pearson", adj = NULL, chains = 4, ...) {
  d <- parse_formula(formula, df)
  
  if(is.null(d$cens_x)) d$cens_x = rep(0, length(d$x))
  if(is.null(d$cens_y)) d$cens_y = rep(0, length(d$y))
  
  d$cens_x_upper = ifelse(d$cens_x %in% -1:1, d$x, d$cens_x_upper)
  d$cens_y_upper = ifelse(d$cens_y %in% -1:1, d$y, d$cens_y_upper)
  
  data = list(
    N       = length(d$x),
    x_lower = d$x,
    x_upper = d$cens_x_upper,
    y_lower = d$y,
    y_upper = d$cens_y_upper,
    cens_x  = d$cens_x,
    cens_y  = d$cens_y
  ) 
  
  init = list(
    mu_x = mean(d$x),
    mu_y = mean(d$y)
  )
  
  if(method == "pearson") {
    if(is.null(adj)) {
      init$rho <= 0
      
      inits <- rep(list(init), chains)
      
      sampling(stanmodels$censored_correlations_interval,
               data = data,
               control = list(adapt_delta = 0.95, max_treedepth = 15),
               chains = chains,
               pars = c("sigma_x", "sigma_y", "mu_x", "mu_y", "rho"),
               init = inits,
               ...)
    }
    else {
      if(class(adj) == "formula") {
        data$z <- eval(parse(text = str_replace(adj, "~", "")), envir = df)
      }
      else {
        data$z <- eval(parse(text = adj), envir = df)
      }
      
      init$mu_z <- mean(data$z);
      init$rho_xy <- 0
      init$rho_xz <- 0
      init$rho_yz <- 0
      
      inits <- rep(list(init), chains)
      
      sampling(stanmodels$censored_correlations_z,
               data = data,
               control = list(adapt_delta = 0.95, max_treedepth = 15),
               chains = chains,
               pars = c("sigma_x", "sigma_y", "sigma_z", "mu_x", "mu_y", "mu_z", "rho_xy", "rho_xz", "rho_yz", "rho_adj"),
               init = inits,
               ...)
    }
  } 
  else if(method == "kendall") {
    data$x_rank <- rank(d$x)
    data$y_rank <- rank(d$y)
    
    sampling(stanmodels$kendalls_tau,
             data = data,
             control = list(adapt_delta = 0.99),
             chains = chains,
             pars = c("rho", "tau"),
             ...)
  }
}
