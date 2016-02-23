mmfitpoisson <- function(x, start) {

  #ll = min(x); up = max(x)
  #coefs = gmmhelper(x, g, start, lower=ll, upper=up)
  lam = mean(x);

  mmf <- mmf(thetahat = lam, thetahatses = NULL, denscomp = NULL, cdfband = NULL);
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
  t <- 1:100
  #plot(t, poisfit(t, lam), type='b', xlim = c(0,100))
  lam
}


