#' Betweenness centralization of weighted network
#' 
#' This function calculates the betweenness centralization of a weighted network.
#' @param edgelist The input weighted edge list.
#' @param directed Logical scalar indicating whether the edge list is directed or undirected. 
#' @param alpha Default is 0.5.
#' @return Betweenness centralization of the specified network.
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
#' centr.betweenness(edg)

centr.betweenness = function(edgelist, directed = FALSE, alpha = 0.5)
{
  if(ncol(edgelist)==2)
  {
    edgelist <- cbind(edgelist, rep(1, nrow(edgelist)))
    warning(cat("The edge list is unweighted\n"))
  }
  bt_w = betweenness_w(edgelist, directed=directed, alpha=alpha)
  bt_w = bt_w[,2]
  max_bt = max(bt_w)
  bt_diff = sum(max_bt - bt_w)
  
  star <- eq.star(edgelist, type="edgelist")
  
  star_bt_w = betweenness_w(star, directed=directed, alpha=alpha)
  star_bt_w = star_bt_w[,2]
  max_star_bt = max(star_bt_w)
  max_bt_diff = sum(max_star_bt - bt_w)
  cen_bt = bt_diff / max_bt_diff
  return(cen_bt)
}
