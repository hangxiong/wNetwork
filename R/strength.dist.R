#' Plot strength distribution of a network
#' 
#' This function plots the strength distribution of a network in the form of edge list.
#' @param edgelist The input edge list.
#' @param type Option indicating whether out- or in-measures to be calculated. The default is "out".
#' The setting is irrelevant for undirected networks, but it must be specified.
#' @param by number: increment of the sequence
#' @param alpha Default is 0.5.
#' @return Return probability density of the strengths of the specified network.
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
#' strength.dist(edg)

strength.dist <- function(edgelist, type = "out", by = 1, alpha = 0.5)
{
  if (!is.edgelist(edgelist)) {
    stop("Not an edge list")
  }
  if(ncol(edgelist) == 2)
  {
    edgelist = cbind(edgelist,rep(1, nrow(edgelist)))
  }
  edgelist = as.tnet(edgelist, type="weighted one-mode tnet")
  st = degree_w(edgelist, measure="output", type=type, alpha=alpha)
  st = st[,2]
  for(i in 1:length(st))
  {
    if(st[i] - floor(st[i]/by) * by < by/2)
    {
      st[i] = floor(st[i]/by) * by
    } else st[i] = floor(st[i]/by + 1) * by
  }
  count = as.matrix(table(st))
  dist = cbind(as.numeric(rownames(count)), count/sum(count))
  rownames(dist) <- NULL
  return(dist)
}
