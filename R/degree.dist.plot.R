#' Plot degree distribution of network
#' 
#' This function plots the degree distribution of a network in the form of edge list.
#' @param edgelist The input weighted edge list.
#' @param type Option that indicating whether out- or in-measures to be calculated. 
#' The default is "out".
#' The setting is irrelevant for undirected networks, but it must be specified.
#' @return Plot of degree distribution of the specified network.
#' @export
#' @examples
#' g <- barabasi.game(1000, directed = FALSE)
#' edg <- get.edgelist(g)
#' degree.dist(edg)
#' degree.dist.plot(edg)

degree.dist.plot <- function(edgelist, type = "out", fit.line = TRUE)
{
  dist = degree.dist(edgelist, type=type)
  if(dist[1,1] == 0)
  {
    degr = dist[,1][-1]
    prob = dist[,2][-1]
  } else
  {
    degr = dist[,1]
    prob = dist[,2]
  }
  plot(prob ~ degr, log = "xy", xlab = "Degree (log)", ylab = "Probability (log)", 
       main = "Degree Distribution")
  if(fit.line)
  {
    reg = lm(log(prob) ~ log(degr))
    cof = coef(reg)
    power.law <- function(x)
    {
      exp(cof[[1]] + cof[[2]] * log(x))
    }
    curve(power.law, col = "red", add = TRUE)
    print(cof)
  }
}