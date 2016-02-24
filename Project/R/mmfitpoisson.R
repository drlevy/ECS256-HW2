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

#' @title poisson
#' @description poisson
#' @export
testpoisson <- function(){
  xb <- rpois(1000, 0.2)
  mmf <- mmfit(xb, "poisson", 1.5 )
  hist(xb, col = "blue", probability = TRUE)
  lam<- mmf$thetahat[1]
  y <- rpois(1000, mmf$thetahat[1])
  hist(y, probability = TRUE, col = "red", add = TRUE)
  return(mmf)
}


