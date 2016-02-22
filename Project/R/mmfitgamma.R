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
