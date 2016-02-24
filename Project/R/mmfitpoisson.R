mmfitpoisson <- function(x, start) {
  n   = length(x);
  lam = mean(x);
  sn2 = sum((x-mean(x))^2)
  se  = sqrt( sn2 / n)

  plot = generateparametricplot(dpois, list(lam), x)
  band = generateecdfplot(x)

  mmf <- mmf(thetahat = lam, thetahatses = se, denscomp = plot, cdfband = band);
  return(mmf)
}

poisfit <- function(x, lam) (lam^x)*exp(-lam)/factorial(x)

#' @title poisson
#' @description poisson
#' @export
testpoisson <- function(){
  xb <- rpois(1000, 0.2)
  mmf <- mmfit(xb, "poisson", 1.5 )
  hist(xb, probability = TRUE)
  lam<- mmf$thetahat[1]
  curve(dpois(x, lam), xlim = c(0,100), add = TRUE)
  return(mmf)
}


