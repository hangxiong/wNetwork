#' Plot degree distribution of network
#' 
#' This function plots the degree distribution of a network in the form of edge list.
#' @param edgelist The input weighted edge list.
#' @param type Option that indicating whether out- or in-measures to be calculated. 
#' The default is "out".
#' The setting is irrelevant for undirected networks, but it must be specified.
#' @return Return probability density of the degrees of the specified network.
#' @export
#' @examples
#' edg <- rbind(
#' c(1,2,4),
#' c(1,3,2),
#' c(2,1,4),
#' c(2,3,4),
#' c(2,4,1),
#' c(2,5,2),
#' c(3,1,2),
#' c(3,2,4),
#' c(4,2,1),
#' c(5,2,2),
#' c(5,6,1),
#' c(6,5,1))
#' degree.dist(edg)

degree.dist <- function(edgelist, type = "out")
{
  if(!(is.edgelist(edgelist))) {
    stop("Not an edge list")
  }
  if(ncol(edgelist) == 2)
  {
    edgelist = cbind(edgelist,rep(1, nrow(edgelist)))
  }
  edgelist = as.tnet(edgelist, type="weighted one-mode tnet")
  dg = degree_w(edgelist, measure="degree", type=type)
  dg = dg[,2]
  count = as.matrix(table(dg))
  dist = cbind(as.numeric(rownames(count)), count/sum(count))
  rownames(dist) <- NULL
  return(dist)
}
