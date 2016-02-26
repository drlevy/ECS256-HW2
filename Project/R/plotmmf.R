#' @importFrom grid grid.layout grid.newpage pushViewport viewport

#' @title plot
#' @description
#'  This function should be called by plot(object) and displays two plots side by side.
#'  The left plot is an empirical cdf with a confidence bound in red and blue around it.
#'  The right plot a graphing of the parametric and non-parametric data.
#' @examples
#'  plot(testbeta())
#' @param x mmf to handle
#' @param ... unused
#' @export
plot.mmf <- function(x, ...) {
  p1 = x$denscomp
  p2 = x$cdfband

  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2)))
  print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
}
