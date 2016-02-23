mmfitbeta <- function(x, start) {
  samplesize = length(x);

  g<-function(th,x){
    t1<-th[1];
    t2<-th[2];
    t12<-t1+t2;
    meanb<-t1/t12;
    m1<-meanb-x;
    m2<-t1*t2/(t12^2*(t12+1))-(x-meanb)^2;
    f<-cbind(m1,m2);return(f);
  }

  coefs = gmmhelper(x, g, start, lower=0 , upper=1)

  mmf <- mmf(thetahat = coefs, thetahatses = NULL, denscomp = NULL, cdfband = NULL);

  return(mmf)
}

betafit <- function(x, a, b) ((1-x)^(b-1))*(x^(a-1))/beta(a,b)

#' @title testbeta
#' @description testbeta
#' @export
testbeta <- function(){
  xb <- rbeta(1000, 0.1, 0.7)
  mmf <- mmfit(xb, "beta", c(0.3, 0.5))
  hist(xb, probability = TRUE)
  a<- mmf$thetahat[1]
  b<- mmf$thetahat[2]
  curve(betafit(x, a, b), xlim = c(0,1), add = TRUE)
}
