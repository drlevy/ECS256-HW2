mmfitfunc <- function(x, g, start, lower = NULL, upper = NULL) {
  mmf <- mmf()

  mmf$thetahat <- gmmhelper(x, g, start, lower, upper)

  return(mmf)
}
