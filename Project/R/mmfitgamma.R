mmfitgamma <- function(x, start) {
  g <- function(th,x){
    a <- th[1];
    s <- th[2];
    mean <- a*s;
    m1 <- mean-x;
    m2 <- (a*(s^2)) - (x - mean)^2;
    f <- cbind(m1, m2)
  }

  ll <- min(x)
  up <- max(x)

  coefs <- gmmhelper(x, g, start, lower=ll, upper=up)

  start[1] <- coefs[1]
  start[2] <- coefs[2]

  plot <- generateparametricplot(gammafit, list(start[1], start[2]), x)
  band <- generateecdfplot(x)

  mmf <- mmf(thetahat = start, thetahatses = coefs[3:4], denscomp = plot, cdfband = band);
}

gammafit <- function(x, a, s) (x^(a-1))*exp(-x/s)/((s^a)*(gamma(a)))

#' @title testgamma
#' @description
#'  This function generates data from a gamma distribution with parameters a=5 and s=1/0.5.
#'  Then uses mmfit to estimate the parameters and graphs the data versus the estimated values.
#' @export
testgamma <- function(){
  x <- rgamma(1000, 5, 1/0.5)

  mmf <- mmfit(x, "gamma", c(a=3, s=1/0.3))
  a <- mmf$thetahat[1]
  s <- mmf$thetahat[2]

  f <- Vectorize(function(x) gammafit(x, 5, 1/0.5))
  curve(f, col="blue", xlim=c(0,100))

  f <- Vectorize(function(x) dgamma(x, a, s))
  curve(f, col="red", xlim=c(0,100), add=TRUE)

  return(mmf)
}
