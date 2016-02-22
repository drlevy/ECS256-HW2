#' @title testpowerlaw
#' @description testpowerlaw
#' @export
testpowerlaw <- function() {
  x <- rpowerlaw(100, 2.7)
  mmf <- mmfit(x, "power_law", 2.1)
  curve(dpowerlaw(x, mmf$thetahat), xlim = c(0,1))
  hist(x/100, probability = TRUE, add = TRUE)
  return(mmf)
}
