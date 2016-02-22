# http://leonidzhukov.net/hse/2014/socialnetworks/lectures/lecture2.pdf
# http://arxiv.org/pdf/0706.1062.pdf
# http://tuvalu.santafe.edu/~aaronc/courses/7000/csci7000-001_2011_L2.pdf
# power law is well-behaved when 2 < k < 3

mmfitpowerlaw <- function(x, start) {
  mmf <- mmf()

  mmf$thetahat <- gmmhelper(x, gpowerlaw, start, 2.01, 2.99)

  return(mmf)
}

gpowerlaw <- function(th, x) {
  t1 <- th[1]
  meanb <- mpowerlaw(x, t1)
  m1 <- meanb-x
  f <- cbind(m1)
  return(f)
}

#' @title testpowerlaw
#' @description testpowerlaw
#' @export
testpowerlaw <- function() {
  x <- rpowerlaw(1000, 2.7)
  mmf <- mmfit(x, "power_law", 2.1)
  hist(x, probability = TRUE)
  minx <- min(x)
  curve(dpowerlaw2(x, mmf$thetahat, minx), add = TRUE)
  return(mmf)
}

mpowerlaw <- function(x, k) min(x)*(k-1)/(k-2)
dpowerlaw2 <- function(x, k, xmin) ((k-1)/xmin)*(x/xmin)^(-k)
dpowerlaw <- function(x, k) ((k-1)/min(x))*(x/(min(x)))^(-k)
ppowerlaw <- function(x, k) 1-(x/min(x))^(1-k)
qpowerlaw <- function(q, k) ((1-q)*min(x)^(1-k))^(1/(1-k))
rpowerlaw <- function(n, k) qpowerlaw(runif(n), k)
