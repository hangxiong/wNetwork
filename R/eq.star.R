#' Create equivalent star network
#' 
#' This function generates the equivalent star network for a given weighted network.
#' @param net A network in the form of adjacency matrix or edge list. 
#' @param type Option that indicates the type of the network. "adjacency": adjacency matrix;
#' "edgelist": edge list
#' @param weighted Logical scalar indicating whether the specified network is weigthed or not.
#' @return Equivalent star network of the specified network in the form of adjacency matrix
#' or edge list, depending on the "type" initially specified.
#' @export
#' @examples
#' net <- rbind(c(1,2,4),
#' c(1,3,2),
#' c(1,5,3),
#' c(2,1,4),
#' c(2,4,1),
#' c(2,6,1),
#' c(3,1,2),
#' c(3,2,4),
#' c(4,2,1),
#' c(4,5,2),
#' c(5,2,2),
#' c(6,5,1))
#' star <- eq.star(net, type = "edgelist")
#' centr.betweenness(net)
#' centr.betweenness(star, directed = FALSE)

eq.star <- function(net, type = "adjacency", weighted = TRUE)
{
  if(type == "adjacency")
  {
    adjacency <- net
    if(is.matrix(adjacency) && is.numeric(adjacency))
    {
      nv = nrow(adjacency)
      star = matrix(0, nv, nv)
      wt = ifelse(weighted, max(adjacency), 1)
      star[2:nv, 1] = rep(wt, nv-1)
      star[1, 2:nv] = rep(wt, nv-1)
      return(star)
    }
    else
      stop("Not an adjacency matrix\n")
  }
  
  if(type == "edgelist")
  {
    edgelist <- net
    if((is.data.frame(edgelist) || is.matrix(edgelist)) 
       && (ncol(edgelist)==2 || ncol(edgelist)==3))
    {
      if(is.numeric(edgelist[,1:2]))
        nv = length(unique(c(edgelist[,1],edgelist[,2])))
      else
        nv = length(unique(c(levels(edgelist[,1]),levels(edgelist[,2]))))
      
      Source = c(rep(1, nv-1), 2:nv)
      Target = c(2:nv, rep(1, nv-1))
      if(ncol(edgelist)==2)
      {
        star = cbind(Source, Target)
        if(weighted)
        {
          warning(cat("Not a weighted network"))
        }
        return(star)
      }
      if(ncol(edgelist)==3)
      {
        if(is.numeric(edgelist[,3]))
        {
          wt <- ifelse(weighted, max(edgelist[,3]), 1)
          weight = rep(wt, length(Source))
          star = cbind(Source, Target, weight)
          return(star)
        }
        else
          stop("Not an adjacency matrix (not all weights are numeric)\n")
      }
    }
    else
      stop("Not an edge list\n")
  }
}