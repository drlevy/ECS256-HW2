#' @title Method of Moment Fit
#' @description
#'  Method of moments estimator with built-in support for common distributions.
#' @param x Sample data.
#' @param g Either the name of a built-in distribution or a g equivalent to \code{\link{gmm}}.
#' @param start Vector of starting guesses for parameters.
#' @param lower Optional lower bound for single parameter estimation.
#' @param upper Optional upper bound for single parameter estimation.
#' @param gd
#'  A function specifying the pmf/pdf for the parametric family being fit.
#'  If the user has specified a built-in family, e.g. the gamma, then this function is NULL.
#' @examples
#'   mmf <- mmfit(x=rpois(1000, 0.2), g="poisson", start=1.5)
#' @seealso
#'  custom distribution: \code{\link{testmmfitfunc}}\cr
#'  "beta": \code{\link{testbeta}}\cr
#'  "poisson": \code{\link{testpoisson}}\cr
#'  "powerlaw": \code{\link{testpowerlaw}}\cr
#'  "gamma": \code{\link{testgamma}}\cr
#'  "mix_two_poisson": \code{\link{testmixtwopoisson}}\cr
#'  "mix_two_exp": \code{\link{testexpmix}}\cr
#' @export
mmfit <- function(x, g, start, lower = NULL, upper = NULL, gd = NULL) {
  mmf <- mmf()

  # Moment function specified.
  if(class(g) == "function") {
    mmf <- mmfitfunc(x, g, gd, start, lower, upper)
  # Built-in distribution.
  } else if(class(g) == "character") {
    if(g == "poisson") {
      mmf <- mmfitpoisson(x, start)
    } else if(g == "powerlaw") {
      mmf <- mmfitpowerlaw(x, start, lower, upper)
    } else if(g == "gamma") {
      mmf <- mmfitgamma(x, start)
    } else if(g == "beta") {
      mmf <- mmfitbeta(x, start)
    } else if(g == "mix_two_poisson") {
      mmf <- mmfitmixtwopoisson(x, start)
    } else if(g == "mix_two_exp") {
      mmf <- mmfitmixtwoexp(x, start)
    }
  }

  return(mmf)
}
