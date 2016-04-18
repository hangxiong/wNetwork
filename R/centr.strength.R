#' Strength centralization of weighted network
#' 
#' This function calculates the strengh centralization of a weighted network.
#' @param edgelist The input weighted edge list.
#' @param type Option that indicating whether out- or in-measures to be calculated. The default is "out".
#' The setting is irrelevant for undirected networks, but it must be specified.
#' @param alpha Default is 0.5.
#' @return Strength centralization of the specified network.
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
#' centr.strength(edg)

centr.strength = function(edgelist, type = "out", alpha = 0.5)
{
  if(ncol(edgelist)==2)
  {
    edgelist <- cbind(edgelist, rep(1, nrow(edgelist)))
    warning(cat("The edge list is unweighted\n"))
  }
  st_w = degree_w(edgelist, measure="output", type=type, alpha=alpha)
  st_w = st_w[,2]
  max_st = max(st_w)
  st_diff = sum(max_st - st_w)
  
  star <- eq.star(edgelist, type="edgelist")
  
  star_st_w = degree_w(star, measure="output", type=type, alpha=alpha)
  star_st_w = star_st_w[,2]
  max_star_st = max(star_st_w)
  max_st_diff = sum(max_star_st - st_w)
  cen_st = st_diff / max_st_diff
  return(cen_st)
}
