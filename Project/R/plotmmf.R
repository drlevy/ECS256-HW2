#' @title plot.mmf
#' @description plot.mmf
#' @export
plot.mmf <- function(obj) {
  p1 = obj$denscomp
  p2= obj$cdfband

  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2)))
  print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
}
