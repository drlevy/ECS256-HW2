
mmfitdiscretepowerlaw <- function(x, start) {
  m <- mean(x)
  gamma <- (1-(2*m))/(1-m)

  mmf <- mmf(thetahat = gamma, thetahatses = NULL, denscomp = NULL, cdfband = NULL)
  return(mmf)
}

#' @title testdiscretepowerlaw
#' @description testdiscretepowerlaw
#' @export
testdiscretepowerlaw <- function() {
  x <- rpldis(n=100000, xmin=1, alpha=2.6)
  mmf <- mmfit(x, "discretepowerlaw", 2.1)
  hist(x, probability = TRUE)
  gamma<- mmf$thetahat[1]
  gamma
}
