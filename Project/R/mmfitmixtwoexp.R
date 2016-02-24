mmfitmixtwoexp <- function(x, start) {
  mmf <- mmf()

  g <- function(th, x)
  {
    r1 <- th[1]
    r2 <- 1.0 - r1
    lambda1 <- th[2]
    lambda2 <- th[3]

    mean <- (r1*(1/lambda1) + r2*(1/lambda2))
    m1 <- mean - x

    expected_value <- 2*r1/(lambda1^2) + 2*r2/(lambda2^2)

    m2 <- expected_value - (x-mean)^2

    m3 <- 6*r1/(lambda1^3) + 6*r2/(lambda2^3) - (x-mean)^3

    f <- cbind(m1, m2, m3)
  }

  coefs <- gmmhelper(x, g, start)

  start[1] <- coefs[1]
  start[2] <- coefs[2]
  start[3] <- coefs[3]

  plot <- generateparametricplot(dexp2, as.list((start)), x)
  band <- generateecdfplot(x)

  mmf <- mmf(thetahat = start, thetahatses = coefs[4:6], denscomp = plot, cdfband = band);
}


#' @title testexpmix
#' @description
#'  This function generates data from two exponential distribution and then uses mmfit to estimate the parameters and then graphs the data versus the estimated values.
#' @examples
#' mmfit = testexpmix()
#' @export
testexpmix <- function() {

  x <- rexp2(1000, 0.3, 5, 2)
  mmf <- mmfit(x, "mix_two_exp", c(r1 = 0.5, lambda1 = 1, lambda2 = 1))
  hist(x, probability = TRUE)
  curve(dexp2(x, mmf$thetahat[1], mmf$thetahat[2], mmf$thetahat[3]), add = TRUE)
  return(mmf)
}

dexp2 <- function(x, r1, lambda1, lambda2)
{
  e <- exp(1)

  return(r1*lambda1/(e^(lambda1*x)) + (1-r1)*lambda2/(e^(lambda2*x)))
}

rexp2 <- function(n, r1, lambda1, lambda2)
{
  e <- exp(1)
  n1 = n*r1
  n2 = n - n1

  return(sample(c( rexp(n1, lambda1), rexp(n2, lambda2))))
}
