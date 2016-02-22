gmmhelper <- function(g, x, start) {
  if(NROW(start) > 1) {
    gmm(g, x, start)$coefficients
  } else {
    gmm(g, x, start, lower = min(0, min(x)), upper = max(start*2, max(x)), method = "Brent")$coefficients
  }
}
