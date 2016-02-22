mmfitpoisson <- function(x, start) {
  samplesize = length(x);

  g<-function(th,x){
    lambda<-th[1];
    mean<-lambda;
    # lambda is mean
    m1<-mean-x;
    return(m1);
  }

  coefs = gmmhelper(x, g, start, lower=0.1, upper=999) # hmm...
  hats = sqrt( res/length(samplesize) );

  mmf <- mmf(thetahat = coefs, thetahatses = hats, denscomp = NULL, cdfband = NULL);
  return(mmf)
}
