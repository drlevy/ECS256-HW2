#' @title summary.mmf
#' @description summary.mmf
#' @export
summary.mmf <- function(obj) {
  output = cbind(EstimatedCoefficients = obj$thetahat, StandardErrors = obj$thetahatses)
  print(output)
}
