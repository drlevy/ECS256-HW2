#' @title plot.mmf
#' @description
#'  This function should be called by plot(object) and displays two plots side by side.
#'  The left plot is an empirical cdf with a confidence bound in red and blue around it.
#'  The right plot a graphing of the parametric and non-parametric data.
#' @examples
#' betadist = testbeta()
#' plot(betadist)
#' @export
plot.mmf <- function(obj) {
  p1 = obj$denscomp
  p2= obj$cdfband

  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2)))
  print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
}
