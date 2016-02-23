
mmfitdiscretepowerlaw <- function(x, start) {
  gamma = mean(x);

  mmf <- mmf(thetahat = lam, thetahatses = NULL, denscomp = NULL, cdfband = NULL);
  return(mmf)
}

#' @title testdiscretepowerlaw
#' @description testdiscretepowerlaw
#' @export
testdiscretepowerlaw <- function() {
  x <- rpowerlaw(1000, 2.7)
  mmf <- mmfit(x, "discretepowerlaw", 2.1)
  hist(x, probability = TRUE)
  gamma<- mmf$thetahat[1]
  gamma
}

qpowerlaw <- function(q, k) ((1-q)*min(q)^(1-k))^(1/(1-k))
rpowerlaw <- function(n, k) qpowerlaw(runif(n), k)
