![Travis CI status](https://travis-ci.org/SilentSpringInstitute/censcor.svg?branch=master)

# censcor
`censcor` is an R package for estimating correlations between censored variables. `censcor` works by fitting a multivariate normal distribution to the data using [Stan](http://mc-stan.org).

## Install

```r
devtools::install_github("silentspringinstitute/censcor", args = "--preclean")
```

## Documentation

Documentation and vignettes for the package are available on the [`censcor` website.](https://silentspringinstitute.github.io/censcor)

## Examples

```r

# Generate 100 x and y values with a 0.5 correlation coefficient and no censoring
df <- generate_censored_data(N = 100, rho = 0.5, L_prob = 0)

fit <- censcor(x ~ y, df)

# Fit dataset with left censoring in both x and y
df <- generate_censored_data(N = 100, rho = 0.5, L_prob = 0.5, direction = -1)

fit <- censcor(x | cens(x_cens) ~ y | cens(y_cens), df)

```

You can find more examples in the [Introduction to censcor](https://silentspringinstitute.github.io/censcor/articles/introduction.html) vignette.