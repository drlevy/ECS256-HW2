#' @title summary
#' @description
#'  This function should be called by summary(object) and prints out the estimated coefficients and the standard errors.
#' @examples
#'  summary(testbeta())
#' @param object mmf to handle
#' @param ... unused
#' @export
summary.mmf <- function(object, ...) {
  output = cbind(EstimatedCoefficients = object$thetahat, StandardErrors = object$thetahatses)
  print(output)
}
