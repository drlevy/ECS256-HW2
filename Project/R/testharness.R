#' @title testpowerlaw
#' @description testpowerlaw
#' @export
testpowerlaw <- function() {
  x <- rpowerlaw(1000, 2.7)
  mmf <- mmfit(x, "power_law", 2.1)
  hist(x, probability = TRUE)
  minx <- min(x)
  curve(dpowerlaw2(x, mmf$thetahat, minx), add = TRUE)
  return(mmf)
}
