#' @title mmfit
#' @description mmfit
#' @export
mmfit <- function(x, g, start, lower = NULL, upper = NULL) {
  mmf <- mmf()

  # Moment function specified.
  if(class(g) == "function") {
    mmf <- mmfitfunc(x, g, start, lower, upper)
  # Built-in distribution.
  } else if(class(g) == "character") {
    if(g == "poisson") {
      mmf <- mmfitpoisson(x, start)
    } else if(g == "discretepowerlaw") {
      mmf <- mmfitdiscretepowerlaw(x, start)
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
