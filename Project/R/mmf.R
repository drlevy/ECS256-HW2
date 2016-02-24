#' @title mmf
#' @description
#'  Output object containing estimates and other convenience functionality.
#' @param thetahat Vector of estimates.
#' @param thetahatses Vector containing standard error of estimates.
#' @param denscomp ggplot graph object comparing distribution with estimated parameters to sample.
#' @param cdfband ggplot graph object with empirical cdf and an enclosing Kolmogorov-Smirnov confidence band.
#' @export
mmf <- function(thetahat = NULL, thetahatses = NULL, denscomp = NULL, cdfband = NULL) {
  mmf <- list(
    thetahat = thetahat,
    thetahatses = thetahatses,
    denscomp = denscomp,
    cdfband = cdfband)
  class(mmf) <- "mmf"
  return(mmf)
}
