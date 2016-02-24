mmfitfunc <- function(x, g, gd, start, lower = NULL, upper = NULL) {
  mmf <- mmf()

  coefs <- gmmhelper(x, g, start, lower, upper)

  start <- coefs[1:length(start)]

  plot <- generateparametricplot(gd, as.list(start), x)
  band <- generateecdfplot(x)

  mmf <- mmf(thetahat = start, thetahatses = coefs[length(start):2*length(start)], denscomp = plot, cdfband = band);
}

#' @title testmmfitfunc
#' @description
#'  This function generates data from a custom implementation of an exponential distribution with lambda=0.5.
#'  Then uses mmfit to estimate the parameters and graphs the data versus the estimated values.
#' @export
testmmfitfunc <- function() {
  x <- rexp(1000, 0.5)
  mmf <- mmfit(x, gexp, 0.1, 0, 1, gd = dexp)
  hist(x, probability = TRUE)
  curve(dexp(x, mmf$thetahat), add = TRUE)
  return(mmf)
}

gexp <- function(th, x) {
  t1 <- th[1]
  meanb <- 1/t1
  m1 <- meanb-x
  f <- cbind(m1)
}
