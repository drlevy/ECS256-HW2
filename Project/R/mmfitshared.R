gmmhelper <- function(x, g, start, lower, upper) {
  if(NROW(start) > 1) {
    summary(gmm(g, x, start))$coefficients
  } else {
    summary(gmm(g, x, start, lower = lower, upper = upper, method = "Brent"))$coefficients
  }
}

generateparametricplot <- function(func, arg, data)
{
  plot = ggplot(data.frame(data), aes(data)) + geom_density(aes(group="Estimation", colour="Parametric Density")) + stat_function(mapping = aes(x), data = data.frame(x=c(min(data),max(data))), fun=func, args = arg, colour = "blue") + theme()
  return(plot)
}
