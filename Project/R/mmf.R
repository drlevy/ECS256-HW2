#' @title mmf
#' @description mmf
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
