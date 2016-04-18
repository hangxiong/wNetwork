#' Plot strength distribution of a network
#' 
#' This function plots the strength distribution of a network in the form of edge list.
#' @param edgelist The input edge list.
#' @param type Option indicating whether out- or in-measures to be calculated. The default is "out".
#' The setting is irrelevant for undirected networks, but it must be specified.
#' @param by number: increment of the sequence
#' @param alpha Default is 0.5.
#' @return Plot of strength distribution of the specified network.
#' @export
#' @examples
#' strength.dist.plot()

strength.dist_plot <- function(edgelist, type = "out", by = 1, alpha = 0.5, fit.line = TRUE)
{
  dist = strength.dist(edgelist, type=type)
  if(dist[1,1] == 0)
  {
    degr = dist[,1][-1]
    prob = dist[,2][-1]
  }
  plot(prob ~ degr, log = "xy", xlab = "Strength (log)", ylab = "Probability (log)", 
       col = 1, main = "Strength Distribution")
  if(fit.line)
  {
    reg = lm(log(prob) ~ log(degr))
    cof = coef(reg)
    power.law <- function(x)
    {
      exp(cof[[1]] + cof[[2]] * log(x))
    }
    curve(power.law, col = "red", add = TRUE, n = length(dg))
    print(cof)
  }
}
