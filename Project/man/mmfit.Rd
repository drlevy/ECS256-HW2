% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mmfit.R
\name{mmfit}
\alias{mmfit}
\title{Method of Moment Fit}
\usage{
mmfit(x, g, start, lower = NULL, upper = NULL, gd = NULL)
}
\arguments{
\item{x}{Sample data.}

\item{g}{Either the name of a built-in distribution or a g equivalent to \code{\link{gmm}}.}

\item{start}{Vector of starting guesses for parameters.}

\item{lower}{Optional lower bound for single parameter estimation.}

\item{upper}{Optional upper bound for single parameter estimation.}

\item{gd}{A function specifying the pmf/pdf for the parametric family being fit.
If the user has specified a built-in family, e.g. the gamma, then this function is NULL.}
}
\description{
Method of moments estimator with built-in support for common distributions.
}
\examples{
  mmf <- mmfit(x=rpois(1000, 0.2), g="poisson", start=1.5)
}
\seealso{
custom distribution: \code{\link{testmmfitfunc}}\cr
 "beta": \code{\link{testbeta}}\cr
 "poisson": \code{\link{testpoisson}}\cr
 "powerlaw": \code{\link{testpowerlaw}}\cr
 "gamma": \code{\link{testgamma}}\cr
 "mix_two_poisson": \code{\link{testmixtwopoisson}}\cr
 "mix_two_exp": \code{\link{testexpmix}}\cr
}

