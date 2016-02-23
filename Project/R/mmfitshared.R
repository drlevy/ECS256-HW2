gmmhelper <- function(x, g, start, lower, upper) {
  if(NROW(start) > 1) {
    summary(gmm(g, x, start))$coefficients
  } else {
    summary(gmm(g, x, start, lower = lower, upper = upper, method = "Brent"))$coefficients
  }
}
