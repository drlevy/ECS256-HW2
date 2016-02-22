#' @title testpowerlaw
#' @description testpowerlaw
#' @export
testpowerlaw <- function() {
  x <- rpowerlaw(1000, 2.7)
  mmf <- mmfit(x, "power_law", 2.1)
  hist(x, probability = TRUE)
  minx <- min(x)
  curve(dpowerlaw2(x, mmf$thetahat, minx), add = TRUE)
  return(mmf)
}

testgamma <- function(){
  xb <- rgamma(1000, 5, 1/0.5)
  mmf <- mmfit(xb, "gamma", c(2.1, 1/0.1))
  hist(xb, probability = TRUE)
  a<- mmf$thetahat[1]
  s<- mmf$thetahat[2]
  curve(dgamma(x, a, s), xlim = c(0,10), add = TRUE)
}
