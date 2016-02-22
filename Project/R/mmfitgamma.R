mmfitgamma <- function(x, start) {
  samplesize = length(x);

  g<-function(th,x){
    alph<-th[1];
    beta<-th[2];
    mean<-alph/beta;
    m1<-mean-x;
    m2<- (alph/(beta^2)) - (x - mean)^2;
    f <- cbind(m1, m2)
    return (f);
  }

  ll = min(x); up = max(x)
  coefs = gmmhelper(x, g, start, lower= ll, upper=up) # maybe?
  hats = sqrt( coefs/length(samplesize) );

  mmf <- mmf(thetahat = coefs, thetahatses = hats, denscomp = NULL, cdfband = NULL);
  return(mmf)
}
