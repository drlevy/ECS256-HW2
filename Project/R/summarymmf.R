#' @title summary.mmf
#' @description
#'  This function should be called by summary(object) and prints out the estimated coefficients and the standard errors.
#' @examples
#' betadist = testbeta()
#' summary(betadist)
#' @export
summary.mmf <- function(obj) {
  output = cbind(EstimatedCoefficients = obj$thetahat, StandardErrors = obj$thetahatses)
  print(output)
}
