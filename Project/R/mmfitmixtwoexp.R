mmfitmixtwoexp <- function(x, start) {

  g <- function(th, x)
  {
    r1 = th[1]
    r2 = 1.0 - r1
    lambda1 = th[2]
    lambda2 = th[3]

    mean = (r1*(1/lambda1) + r2*(1/lambda2))
    m1 =  mean - x

    expected_value = 2*r1/(lambda1^2) + 2*r2/(lambda2^2)

    m2 = expected_value - (x-mean)^2

    m3 = 6*r1/(lambda1^3) + 6*r2/(lambda2^3) - (mean-x)^3

    f = cbind(m1, m2, m3)
    return(f)
  }

  coefs = gmmhelper(x, g, start, 0, 0)
  standard_error = 0

  mmf <- mmf(thetahat = coefs, thetahatses = standard_error, denscomp = NULL, cdfband = NULL);
  return(mmf)
}


#' @title testexpmix
#' @description testexpmix
#' @export
testexpmix <- function() {

  x <- rexp2(10000, 0.3, 5, 0.2)
  mmf <- mmfit(x, "mix_two_exp", c(r1 = 0.5, lambda1 = 1, lambda2 = 1))
  hist(x, probability = TRUE)
  curve(dexp2(x, mmf$thetahat[1], mmf$thetahat[2], mmf$thetahat[3]), add = TRUE)

  return(mmf)
}

dexp2 <- function(x, r1, lambda1, lambda2)
{
  e <- exp(1)
  if( runif(1) <= r1 )
  {
    return(lambda1/(e^(lambda1*x)))
  }
  else
  {
    return(lambda2/(e^(lambda2*x)))
  }
}

rexp2 <- function(n, r1, lambda1, lambda2)
{
  e <- exp(1)
  n1 = n*r1
  n2 = n - n1

  return(sample(c( rexp(n1, lambda1), rexp(n2, lambda2))))
}
