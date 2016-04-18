#' Degree centralization of weighted network
#' 
#' This function calculates the degree centralization of a weighted network.
#' @param edgelist The input weighted edge list.
#' @param type Option indicating whether out- or in-measures to be calculated. The default is "out".
#' The setting is irrelevant for undirected networks, but it must be specified.
#' @param alpha Default is 0.5.
#' @return Degree centralization of the specified network.
#' @export
#' @examples
#' edg <- rbind(c(1,2,4),
#' c(1,3,2),
#' c(1,5,3),
#' c(2,1,4),
#' c(2,3,4),
#' c(2,4,1),
#' c(2,5,2),
#' c(2,6,1),
#' c(3,1,2),
#' c(3,2,4),
#' c(3,5,3),
#' c(4,2,1),
#' c(4,5,2),
#' c(5,2,2),
#' c(5,6,1),
#' c(6,5,1))
#' centr.degree(edg)

centr.degree = function(edgelist, type = "out", alpha = 0.5)
{
  if(ncol(edgelist)==2)
  {
    edgelist <- cbind(edgelist, rep(1, nrow(edgelist)))
    warning(cat("The edge list is unweighted\n"))
  }
  dg_w = degree_w(edgelist, measure="degree", type=type, alpha=alpha)
  dg_w = dg_w[,2]
  max_dg = max(dg_w)
  dg_diff = sum(max_dg - dg_w)
  
  star <- eq.star(edgelist, type="edgelist")
  
  star_dg_w = degree_w(star, measure="degree", type=type, alpha=alpha)
  star_dg_w = star_dg_w[,2]
  max_star_dg = max(star_dg_w)
  max_dg_diff = sum(max_star_dg - dg_w)
  cen_dg = dg_diff / max_dg_diff
  return(cen_dg)
}
