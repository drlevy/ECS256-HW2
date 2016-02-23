mmfitmixtwopoisson <- function(x, start) {
  mmf <- mmf()

  sm <- rep(0, 3)
  sm[1] <- (1/length(x))*sum(x)
  sm[2] <- (1/length(x))*sum(x^2)
  sm[3] <- (1/length(x))*sum(x^3)

  f <- function(th) {
    fval <- rep(0, 3)

    th1 <- th[1]
    th2 <- th[2]
    th3 <- th[3]

    fval[1] <- th1*th2+(1-th1)*th3-sm[1]
    fval[2] <- th1*(th2+th2^2)+(1-th1)*(th3+th3^2)-sm[2]
    fval[3] <- th1*(th2+3*th2^2+th2^2)+(1-th1)*(th3+3*th3^2+th3^3)-sm[3]

    fval
  }

  mmf$thetahat <- nleqslv(start, f)$x

  return(mmf)
}
