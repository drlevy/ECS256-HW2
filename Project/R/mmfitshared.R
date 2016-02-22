gmmhelper <- function(g, x, start) {
  if(NROW(start) > 1) {
    gmm(g, x, start)$coefficients
  } else {
    # TODO: add better scaling of boundaries.
    gmm(g, x, start, lower = 0, upper = start*2, method = "Brent")$coefficients
  }
}
