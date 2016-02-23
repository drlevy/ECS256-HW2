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

  #hats = c(r = coefs[1], Lambda1 = coefs[2], Lambda2 = coefs[3])
  start[1] = coefs[1]
  start[2] = coefs[2]
  start[3] = coefs[3]
  se = c(coefs[4],coefs[5],coefs[6])

  mmf <- mmf(thetahat = start, thetahatses = se, denscomp = NULL, cdfband = NULL);
  return(mmf)
}


#' @title testexpmix
#' @description testexpmix
#' @export
testexpmix <- function() {

  x <- rexp2(1000, 0.3, 20, 0.1)
  mmf <- mmfitmixtwoexp(x, c(r1 = 0.5, lambda1 = 1, lambda2 = 1))
  hist(x, probability = TRUE)
  curve(dexp2(x, mmf$thetahat[1], mmf$thetahat[2], mmf$thetahat[3]), add = TRUE)
  #denscomphelper(x, dexp2(x, mmf$thetahat[1], mmf$thetahat[2], mmf$thetahat[3]))
 # plot(ecdf(x))
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
