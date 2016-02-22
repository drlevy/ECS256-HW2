gmmhelper <- function(x, g, start, lower, upper) {
  if(NROW(start) > 1) {
    gmm(g, x, start)$coefficients
  } else {
    gmm(g, x, start, lower = lower, upper = upper, method = "Brent")$coefficients
  }
}
