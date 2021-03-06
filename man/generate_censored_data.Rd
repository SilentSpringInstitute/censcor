% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_censored_data.R
\name{generate_censored_data}
\alias{generate_censored_data}
\title{Generate random censored data}
\usage{
generate_censored_data(N, rho, L_prob, direction = c(-1), adj = FALSE,
  adj_sigma = NULL)
}
\arguments{
\item{N}{number of rows}

\item{rho}{true correlation}

\item{L_prob}{percentage of data to censor}

\item{direction}{types of censoring to include; acceptable values are -1 (left), 1 (right), 2 (interval) or any combination}

\item{adj}{whether to adjust values by a normally distributed variable}

\item{adj_sigma}{if adj is true, this specifies the standard deviation of the adjustment variable}
}
\value{
data frame
}
\description{
Generate random censored data
}
\details{
When multiple types of censoring are requested, the function evenly splits the censored values across all the types.
Interval censoring is always done around the center of the distribution.
}
\examples{

# Generate 100 x and y values with a 0.5 correlation and 50\% left-censored
generate_censored_data(100, rho = 0.5, L_prob = 0.5, direction = -1)

# Generate 500 x and y values with a -0.25 correlation and 25\% right and interval censored
generate_censored_data(500, rho = -0.25, L_prob = 0.25, direction = c(1, 2))

}
