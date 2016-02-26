#' @importFrom gmm gmm
#' @importFrom ggplot2 ggplot geom_density aes stat_function theme stat_ecdf

gmmhelper <- function(x, g, start, lower, upper) {
  if(NROW(start) > 1) {
    summary(gmm(g, x, start))$coefficients
  } else {
    summary(gmm(g, x, start, lower = lower, upper = upper, method = "Brent"))$coefficients
  }
}

generateparametricplot <- function(func, arg, data)
{
  x<-c(min(data),max(data))
  plot <- ggplot(data.frame(data), aes(data)) + geom_density(aes(group="Estimation", colour="Parametric Density")) + stat_function(mapping = aes(x), data = data.frame(x), fun=func, args = arg, colour = "blue") + theme()
}

generateecdfplot <- function(data)
{
  ecdfd <- ecdf(data)

  upperf <- function(index, ecdfd, n)
  {
    return(ecdfd(index) + 1.358/sqrt(n))
  }

  lowerf <- function(index, ecdfd, n)
  {
    return(ecdfd(index) - 1.358/sqrt(n))
  }

  x<-c(0,max(data))
  plot <- ggplot(data.frame(data), aes(data)) + stat_ecdf(geom = "step") + stat_function(data = data.frame(x), mapping = aes(x), fun=upperf, args = list(ecdfd, length(data)), colour = "blue") + stat_function(data = data.frame(c(0,max(data))), mapping = aes(x), fun=lowerf, args = list(ecdfd, length(data)), colour = "red")
}
