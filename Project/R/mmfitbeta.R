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

  start[1] = coefs[1]
  start[2] = coefs[2]
  se = c(coefs[3], coefs[4])

  mmf <- mmf(thetahat = start, thetahatses = se, denscomp = NULL, cdfband = NULL);

  return(mmf)
}

#' @title testbeta
#' @description testbeta
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


plot = ggplot(data.frame(xb), aes(xb)) + geom_histogram(binwidth = 0.01) + stat_function(mapping = aes(x), data = data.frame(x=c(0,2)), fun=dbeta, args = list(5,50))

plot2 = ggplot(data.frame(x=c(0, 2)), aes(x)) + stat_function(fun=dbeta, args = list(5,50)) + geom_histogram(mapping=aes(xb), data=data.frame(xb), binwidth = 0.01)
