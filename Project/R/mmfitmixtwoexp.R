mmfitmixtwoexp <- function(x, start) {

  g <- function(th, x)
  {
    r1 = th[1]
    r2 = 1.0 - r1
    lambda1 = th[2]
    lambda2 = th[3]

    mean = (r1*(1/lambda1) + r2*(1/lambda2))
    m1 =  mean - x

  #  m2 = r1/(lambda1^2) + r2/(lambda2^2) - (x-mean)^2
  #  m2 = (r1*(1/lambda1 + 1/lambda1^2)) + r2*(1/lambda2 + 1/lambda2^2) - mean*mean - (x-mean)^2

   # m3 = mean*mean - x*x

 #   m4 = mean*mean*mean - x*x*x

    f = cbind(m1)
    return(f)
  }

  coefs = gmmhelper(x, g, start, 0,100)
 # coefs = c(r1=coefs[1], r2 = 1.0 - coefs[1], lambda1 = coefs[2], lambda2 = coefs[3])
  standard_error = 0

  mmf <- mmf(thetahat = coefs, thetahatses = standard_error, denscomp = NULL, cdfband = NULL);
  return(mmf)
}

testexpmix <- function() {
  dexp2 <- function(x, r1, lambda1, lambda2)
  {
    e <- exp(1)
    if( runif(1) <= r1 )
    {
      return(lambda1*1/(e^(lambda1*x)))
    }
    else
    {
      return(lambda2*1/(e^(lambda2*x)))
    }
  }

  rexp2 <- function(n, r1, lambda1, lambda2)
  {
    e <- exp(1)
    n1 = n*r1
    n2 = n - n1

    return(sample(c( rexp(n1, lambda1), rexp(n2, lambda2))))
  }

  x <- rexp2(1000, 0.3, 3, 0.5)
 # x = rexp(1000, 1.5)
  mmf <- mmfit(x, "mix_two_exp", c(r1 = 0.5, lambda1 = 1, lambda2 = 1))
  hist(x, probability = TRUE)
 # curve(dexp2(x, 0.3, 0.7, 1.5, 0.5), add = TRUE)
  curve(dexp2(x, mmf$thetahat[1], mmf$thetahat[2], mmf$thetahat[3]), add = TRUE)
#  curve(dexp(x, mmf$thetahat[1]), add = TRUE)
  return(mmf)
}
