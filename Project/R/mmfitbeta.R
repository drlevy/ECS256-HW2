mmfitbeta <- function(x, start) {
  samplesize <- length(x);

  g<-function(th,x){
    t1<-th[1];
    t2<-th[2];
    t12<-t1+t2;
    meanb<-t1/t12;
    m1<-meanb-x;
    m2<-t1*t2/(t12^2*(t12+1))-(x-meanb)^2;
    f<-cbind(m1,m2);
  }

  coefs = gmmhelper(x, g, start, lower=0 , upper=1)

  start[1] = coefs[1]
  start[2] = coefs[2]

  plot <- generateparametricplot(dbeta, as.list(start), x)
  band <- generateecdfplot(x)

  mmf <- mmf(thetahat = start, thetahatses = coefs[3:4], denscomp = plot, cdfband = band);

  return(mmf)
}

#' @title testbeta
#' @description
#'  This function generates data from a beta distribution with parameters 5 and 20.
#'  Then uses mmfit to estimate the parameters and graphs the data versus the estimated values.
#' @examples
#' mmfit = testbeta()
#' @export
testbeta <- function(){
  xb <- rbeta(1000, 5, 20)
  mmf <- mmfit(xb, "beta", c(alpha = 3, beta = 10))
  hist(xb, probability = TRUE)
  a<- mmf$thetahat[1]
  b<- mmf$thetahat[2]
  curve(dbeta(x, a, b), xlim = c(0,1), add = TRUE)
  return(mmf)
}
