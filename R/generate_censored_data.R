#' Generate random censored data
#' 
#' @param N number of rows
#' @param rho true correlation
#' @param L_prob percentage of data to censor
#' @param direction types of censoring to include; acceptable values are -1 (left), 1 (right), 2 (interval) or any combination
#' @param adj whether to adjust values by a normally distributed variable
#' @param adj_sigma if adj is true, this specifies the standard deviation of the adjustment variable
#' 
#' @details 
#' When multiple types of censoring are requested, the function evenly splits the censored values across all the types.
#' Interval censoring is always done around the center of the distribution.
#' 
#' @return data frame
#' 
#' @examples 
#' 
#' # Generate 100 x and y values with a 0.5 correlation and 50% left-censored
#' generate_censored_data(100, rho = 0.5, L_prob = 0.5, direction = -1)
#' 
#' # Generate 500 x and y values with a -0.25 correlation and 25% right and interval censored
#' generate_censored_data(500, rho = -0.25, L_prob = 0.25, direction = c(1, 2))
#' 
#' @export
generate_censored_data <- function(N, rho, L_prob, direction = c(-1), adj = FALSE, adj_sigma = NULL) {
  sigma <- matrix(
    c(1, rho, rho, 1),
    ncol = 2, byrow = T
  )
  mu <- rep(0, 2)
  
  X <- MASS::mvrnorm(n = N, mu, sigma)
  
  if(adj == TRUE) {
    z <- rnorm(N, 0, adj_sigma)
    X = X
  }
  
  X_cens <- X
  X_cens_upper <- X
  
  if(identical(direction, c(-1))) {
    L <- quantile(X[,1:2], probs = c(L_prob)) 
    cens <- ifelse(X_cens < L, -1, 0)
    X_cens[cens == -1] <- L
    X_cens_upper = X_cens
  }
  else if(identical(direction, c(1))) {
    L <- quantile(X[,1:2], probs = c(1 - L_prob)) 
    cens <- ifelse(X_cens > L, 1, 0)
    X_cens[cens == 1] <- L
    X_cens_upper = X_cens
  }
  else if(identical(direction, c(2))) {
    L <- quantile(X[,1:2], probs = c(0.5 - L_prob / 2, 0.5 + L_prob / 2)) 
    cens <- ifelse(X_cens > L[1] & X_cens < L[2], 2, 0)
    X_cens[cens == 2] <- L[1]
    X_cens_upper = X_cens
    X_cens_upper[cens == 2] <- L[2]
  }
  else if(identical(sort(direction), c(-1, 1))) {
    L <- quantile(X[,1:2], probs = c(L_prob / 2, 1 - L_prob / 2))
    cens <- ifelse(X_cens < L[1], -1, ifelse(X_cens > L[2], 1, 0))
    X_cens[cens == -1] <- L[1]
    X_cens[cens ==  1] <- L[2]
  }
  else if(identical(sort(direction), c(-1, 2))) {
    L <- quantile(X[,1:2], probs = c(L_prob / 2, 0.5 - L_prob / 4, 0.5 + L_prob / 4))
    cens <- ifelse(X_cens < L[1], -1, ifelse(X_cens > L[2] & X_cens < L[3], 2, 0))
    X_cens[cens == -1] <- L[1]
    X_cens[cens ==  2] <- L[2]
    X_cens_upper = X_cens
    X_cens_upper[cens == 2] <- L[3]
  }
  else if(identical(sort(direction), c(1, 2))) {
    L <- quantile(X[,1:2], probs = c(0.5 - L_prob / 4, 0.5 + L_prob / 4, 1 - L_prob / 2))
    cens <- ifelse(X_cens > L[3], 1, ifelse(X_cens > L[1] & X_cens < L[2], 2, 0))
    X_cens[cens == 1] <- L[3]
    X_cens[cens == 2] <- L[1]
    X_cens_upper = X_cens
    X_cens_upper[cens == 2] <- L[2]
  }
  else if(identical(sort(direction), c(-1, 1, 2))) {
    L <- quantile(X[,1:2], probs = c(L_prob / 3, 0.5 - L_prob / 6, 0.5 + L_prob / 6, 1 - L_prob / 3)) 
    cens <- ifelse(X_cens < L[1], -1,
            ifelse(X_cens > L[2] & X_cens < L[3], 2,
            ifelse(X_cens > L[4], 1, 0)))
    X_cens[cens == -1] <- L[1]
    X_cens[cens ==  1] <- L[4]
    X_cens[cens == 2] <- L[2]
    X_cens_upper = X_cens
    X_cens_upper[cens == 2] <- L[3]
  }
  
  res <- data.frame(
    x_true = X[,1],
    x = X_cens[,1],
    x_upper = X_cens_upper[,1],
    x_cens = cens[,1],
    y_true = X[,2],
    y = X_cens[,2],
    y_upper = X_cens_upper[,2],
    y_cens = cens[,2]
  )
  
  if(adj == TRUE) {
    res$adj = z
  }
  
  res
}
