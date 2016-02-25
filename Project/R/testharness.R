#' @title doTests
#' @description
#'  Runs all test examples and returns a list of \code{\link{mmf}} objects.
#' @export
doTests <- function() {
  mmfs <- list()

  mmfs$beta <- testbeta()
  mmfs$gamma <- testgamma()
  mmfs$poisson <- testpoisson()
  mmfs$powerlaw <- testpowerlaw()
  mmfs$mixtwoexp <- testexpmix()
  mmfs$mixtwopoisson <- testmixtwopoisson()
  mmfs$mmfitfunc <- testmmfitfunc()

  return(mmfs)
}
