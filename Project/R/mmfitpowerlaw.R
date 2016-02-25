mmfitpowerlaw <- function(x, start, lower, upper, crit=10e-6, itermax=10e9) {
  g <- function(th, x) {
    t1 <- th[1]

    c <- 0
    cprev <- 0

    EX <- 0
    EXprev <- 0

    i <- 1
    repeat {
      if (i > itermax) {
        break
      }

      firstItr <- i == 1
      skipC <- !firstItr && abs(c - cprev) < crit
      skipEX <- !firstItr && abs(EX - EXprev) < crit

      if (skipC && skipEX) {
        break
      }

      if (!skipC) {
        cprev <- c
        c <- c + i^(-t1)
      }

      if (!skipEX) {
        EXprev <- EX
        EX <- EX + i^(1-t1)
      }

      i <- i + 1
    }
    c <- 1/c
    EX <- c*EX

    m1 <- EX-x

    f <- cbind(m1)
  }

  coefs <- gmmhelper(x, g, start, lower, upper)

  start[1] <- coefs[1]

  plot <- generateparametricplot(dpldis, as.list(start), x)
  band <- generateecdfplot(x)

  mmf <- mmf(thetahat = start, thetahatses = coefs[4:6], denscomp = plot, cdfband = band);
}

#' @title testpowerlaw
#' @description
#'  This function generates data from a discrete power law distribution with gamma=2.2.
#'  Then uses mmfit to estimate the parameters and graphs the data versus the estimated values.
#' @export
testpowerlaw <- function() {
  x <- rpldis(1000, xmin=2, alpha=2.2)

  mmf <- mmfit(x=x, g="powerlaw", start=2.5, lower=2, upper=3)

  f <- Vectorize(function(x) dpldis(x, xmin=2, alpha=2.2))
  curve(f, col="blue", xlim=c(0,100))

  f <- Vectorize(function(x) dpldis(x, xmin=2, alpha=mmf$thetahat[1]))
  curve(f, col="red", xlim=c(0,100), add=TRUE)

  return(mmf)
}
