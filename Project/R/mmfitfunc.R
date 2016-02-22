mmfitfunc <- function(x, g, start, lower = NULL, upper = NULL) {
  mmf <- mmf()

  mmf$thetahat <- gmmhelper(x, g, start, lower, upper)

  return(mmf)
}

#' @title testmmfitfunc
#' @description testmmfitfunc
#' @export
testmmfitfunc <- function() {
  x <- rexp(1000, 0.5)
  mmf <- mmfit(x, gexp, 0.1, 0, 1)
  hist(x, probability = TRUE)
  minx <- min(x)
  curve(dexp(x, mmf$thetahat), add = TRUE)
  return(mmf)
}

gexp <- function(th, x) {
  t1 <- th[1]
  meanb <- 1/t1
  m1 <- meanb-x
  f <- cbind(m1)
  return(f)
}
