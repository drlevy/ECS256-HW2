#' @title print.mmf
#' @description print.mmf
#' @export
print.mmf <- function(obj) {
  output = cbind(EstimatedCoefficients = obj$thetahat)
  print(output)
}
