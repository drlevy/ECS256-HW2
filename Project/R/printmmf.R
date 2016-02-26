#' @title print
#' @description
#'  This function should be called by print(object) and prints out the estimated coefficients.
#' @examples
#'  print(testbeta())
#' @param x mmf to handle
#' @param ... unused
#' @export
print.mmf <- function(x, ...) {
  output = cbind(EstimatedCoefficients = x$thetahat)
  print(output)
}
