# power law is well-behaved when 2 < k < 3

mmfitpowerlaw <- function(x, start) {
  mmf <- mmf()

  mmf$thetahat <- gmmhelper(x, gpowerlaw, start, 2.01, 2.99)

  return(mmf)
}

gpowerlaw <- function(th, x) {
  t1 <- th[1]
  meanb <- mpowerlaw(t1)
  m1 <- meanb-x
  f <- cbind(m1)
  return(f)
}

mpowerlaw <- function(k) (k-1)/(k-2)
ppowerlaw <- function(x, k) (k-1)*x^(-k)
dpowerlaw <- function(x, k) x^(1-k)
qpowerlaw <- function(q, k) k^(1/(1-q))
rpowerlaw <- function(n, k) qpowerlaw(runif(n), k)
