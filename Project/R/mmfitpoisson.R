mmfitpoisson <- function(x, start) {
  n <- length(x);
  lam <- mean(x);
  se <- sqrt(lam / n)

  plot <- generateparametricplot(dpois, list(lam), x)
  band <- generateecdfplot(x)

  start[1] <- lam

  mmf <- mmf(thetahat = start, thetahatses = se, denscomp = plot, cdfband = band);
  return(mmf)
}

poisfit <- function(x, lam) (lam^x)*exp(-lam)/factorial(x)

#' @title testpoisson
#' @description
#'  This function generates data from a poisson distribution with lambda=0.2.
#'  Then uses mmfit to estimate lambda and graphs the data versus the estimated values.
#' @examples
#' mmfit <- testpoisson()
#' @export
testpoisson <- function(){
  x <- rpois(1000, 0.2)
  mmf <- mmfit(x, "poisson", c(l=1.5))
  hist(x, col = "blue", probability = TRUE)
  lam <- mmf$thetahat[1]
  y <- rpois(1000, mmf$thetahat[1])
  hist(y, probability = TRUE, col = "red", add = TRUE)
  return(mmf)
}


