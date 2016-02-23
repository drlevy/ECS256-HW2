mmfitgamma <- function(x, start) {
  samplesize = length(x);

  g<-function(th,x){
    a<-th[1];
    s<-th[2];
    mean<-a*s;
    m1<-mean-x;
    m2<- (a*(s^2)) - (x - mean)^2;
    f <- cbind(m1, m2)
    return (f);
  }

  ll = min(x); up = max(x)
  coefs = gmmhelper(x, g, start, lower= ll, upper=up) # maybe?
  hats = sqrt( coefs/length(samplesize) );

  mmf <- mmf(thetahat = coefs, thetahatses = hats, denscomp = NULL, cdfband = NULL);
  return(mmf)
}


gammafit <- function(x, a, s) (x^(a-1))*exp(-x/s)/((s^a)*(gamma(a)))

#' @title testgamma
#' @description testgamma
#' @export
testgamma <- function(){
  xb <- rgamma(1000, 5, 1/0.5)
  mmf <- mmfit(xb, "gamma", c(2.1, 1/0.1))
  hist(xb, probability = TRUE)
  a<- mmf$thetahat[1]
  s<- mmf$thetahat[2]
  curve(gammafit(x, a, s), xlim = c(0,10), add = TRUE)
}
