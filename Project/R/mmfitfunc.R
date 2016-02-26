mmfitfunc <- function(x, g, gd, start, lower = NULL, upper = NULL) {
  mmf <- mmf()

  coefs <- gmmhelper(x, g, start, lower, upper)

  startlist <- list()
  for (i in 1:length(start)) {
    start[i] <- coefs[i]
    startlist[[i]] <- start[i]
  }

  plot <- generateparametricplot(gd, startlist, x)
  band <- generateecdfplot(x)

  mmf <- mmf(thetahat = start, thetahatses = coefs[(length(start)+1):(2*length(start))], denscomp = plot, cdfband = band);
}

#' @title testmmfitfunc
#' @description
#'  This function generates data from a custom implementation of an exponential distribution with lambda=0.5.
#'  Then uses mmfit to estimate the parameters and graphs the data versus the estimated values.
#' @export
testmmfitfunc <- function() {
  x <- rexp(1000, 0.5)

  mmf <- mmfit(x, g=gexp, start=c(l=0.5), lower=0, upper=1, gd = dexp)

  f <- Vectorize(function(x) dexp(x, 0.5))
  curve(f, col="blue", xlim=c(0,100))

  f <- Vectorize(function(x) dexp(x, mmf$thetahat))
  curve(f, col="red", xlim=c(0,100), add=TRUE)

  return(mmf)
}

gexp <- function(th, x) {
  t1 <- th[1]
  meanb <- 1/t1
  m1 <- meanb-x
  f <- cbind(m1)
}
