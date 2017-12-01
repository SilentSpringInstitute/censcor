.onLoad <- function(libname, pkgname) {
  if(length(names(stanmodels)) > 0) {
    modules <- paste0("stan_fit4", names(stanmodels), "_mod")
    for (m in modules) loadModule(m, what = TRUE)
  }
}
