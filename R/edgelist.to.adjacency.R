#' Convert edge list into adjacency matrix
#'
#' This funciton converts a weighted or unweighted edge list into an adjacency matrix.
#' @param edgelist The input edge list.
#' @return Returns an adjacency matrix of the specified network.
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
#' edgelist.to.adjacency(edg)

edgelist.to.adjacency <- function(edgelist)
{
  if(ncol(edgelist)==2 || ncol(edgelist)==3)
  {
    nr = nrow(edgelist)
    if(ncol(edgelist)==2)
      edgelist <- cbind(edgelist, rep(1, nr))
    if(is.numeric(edgelist[,3]))
    {
      if(is.numeric(edgelist[,1:2]))
        nodes <- unique(c(edgelist[,1],edgelist[,2]))
      else {
        nodes <- unique(c(levels(edgelist[,1]),levels(edgelist[,2])))
      } 
      n <- length(nodes)
      adj <- matrix(data=0, nrow=n, ncol=n)
      for(i in 1:nr)
      {
        rindex = which(nodes==edgelist[i,1])
        cindex = which(nodes==edgelist[i,2])
        adj[rindex,cindex] = edgelist[i,3]
      }
      return(adj)
    }
    else {
      stop("Not an edge list (not all weights are numeric)\n")
    }
  }
  else {
    stop("Not an edge list\n")
  }
}