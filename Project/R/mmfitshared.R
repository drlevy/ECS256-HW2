gmmhelper <- function(g, x, start) {
  if(NCOL(start) > 1) {
    gmm(g, x, start)$coefficients
  } else {
    gmm(g, x, start, lower = 0, upper = 10, method = "Brent")$coefficients
  }
}
