gmmhelper <- function(x, g, start, lower = NULL, upper = NULL) {
  if(NROW(start) > 1) {
    gmm(g, x, start)$coefficients
  } else {
    if(!is.null(lower) && !is.null(upper)) {
      gmm(g, x, start, lower = lower, upper = upper, method = "Brent")$coefficients
    } else {
      stop("Must provide bounds for single parameter estimation.")
    }
  }
}
