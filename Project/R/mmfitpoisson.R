mmfitpoisson <- function(x, start) {
  n   = length(x);
  lam = mean(x);
  se  = sqrt( lam / n)

  plot = generateparametricplot(dpois, list(lam), x)
  band = generateecdfplot(x)
  coefs= c(Estimate=lam, "Std. Error"=se)

  mmf <- mmf(thetahat = coefs, thetahatses = se, denscomp = plot, cdfband = band);
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
  #curve(dpois(x, lam), xlim = c(0,100), add = TRUE)
  return(mmf)
}


