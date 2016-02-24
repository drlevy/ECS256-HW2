mmfitmixtwopoisson <- function(x, start) {
  mmf <- mmf()

  g <- function(th, x) {
    r <- th[1]
    l1 <- th[2]
    l2 <- th[3]

    EX <- r*l1+(1-r)*l2
    EX2 <- r*(l1+l1^2)+(1-r)*(l2+l2^2)
    EX3 <- r*(l1+3*l1^2+l1^3)+(1-r)*(l2+3*l2^2+l2^3)

    m1 <- EX-x
    m2 <- EX2-(EX)^2-(x-EX)^2
    m3 <- EX3-3*EX*EX2+2*(EX)^2*EX+(EX)^2-(x-EX)^3

    f <- cbind(m1, m2, m3)
  }

  start[1] <- coefs[1]
  start[2] <- coefs[2]
  start[3] <- coefs[3]

  plot <- generateparametricplot(dpois2, as.list(start), x)
  band <- generateecdfplot(x)

  mmf <- mmf(thetahat = start, thetahatses = coefs[4:6], denscomp = plot, cdfband = band);
}

#' @title testmixtwopoisson
#' @description testmixtwopoisson
#' @export
testmixtwopoisson <- function() {
  x <- rpois2(1000, 0.5, 5, 10)
  mmf <- mmfit(x, "mix_two_poisson", c(r = 0.3, l1 = 2, l2 = 5))
  hist(x, col = "blue", probability = TRUE)
  y <- rpois2(1000, mmf$thetahat[1], mmf$thetahat[2], mmf$thetahat[3])
  hist(y, probability = TRUE, col = "red", add = TRUE)
  return(mmf)
}

dpois2 <- function(x, r, l1, l2)
{
  r*dpois(x, l1)+(1-r)*dpois(x, l2)
}

rpois2 <- function(n, r, l1, l2)
{
  n1 <- n*r
  n2 <- n-n1
  return(sample(c(rpois(n1, l1), rpois(n2, l2))))
}
