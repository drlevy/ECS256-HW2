#' @title print.mmf
#' @description This function should be called by pint(object) and prints out the estimated coefficients.
#' @examples
#' betadist = testbeta()
#' print(betadist)
#' @export
print.mmf <- function(obj) {
  output = cbind(EstimatedCoefficients = obj$thetahat)
  print(output)
}
