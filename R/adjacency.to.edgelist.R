#' Convert adjacency matrix to edge list
#'
#' This function converts a weighted or unweighted adjacency matrix to an edge list.
#' @param adjacency The input adjacency matrix.
#' @param directed Logical scalar indicating whether the network is directed or undirected.
#' Default is NULL, which means that the function checks whether the adjacency matrix is directed or not.
#' @param weighted Logical scalar indicating whether the network is weighted or unweighted.
#' Default is NULL, which means that the function checks whether the adjacency matrix is unweighted or not.
#' @return Returns a matrix with two or three columns. The first column contains the ids of source nodes 
#' in the specified network, and the second column contains the ids of target nodes.
#' The third column, specific for a weighted network, contains the weights of edges.
#' @export 
#' @examples
#' adj <- matrix(sample(c(0:4), 25, replace=TRUE), nr=5)
#' adjacency.to.edgelist(adj)

adjacency.to.edgelist <- function(adjacency, directed = NULL, weighted = NULL)
{
  dimnames(adjacency) <- NULL   # Remove rownames and colnames
  if(is.matrix(adjacency) && is.numeric(adjacency))
  {
    ## Check if the network is directed or not
    if(isSymmetric(adjacency))
    {
      if(is.null(directed))
        directed <- FALSE
      else
      {
        if(directed)
        {
          directed <- FALSE
          warning(cat("The network is undirected\n"))
        }
      }
    }
    else
    {
      if(is.null(directed))
        directed <- TRUE
      else
        if(!directed)
        {
          directed <- TRUE
          warning(cat("The network is directed\n"))
        }
    }
    
    ## Check if the network is weighted or not
    if(all(adjacency %in% c(0,1)))
    {
      if(is.null(weighted))
        weighted <- FALSE
      else
      {
        if(weighted)
        {
          weighted <- FALSE
          warning(cat("The network is unweighted\n"))
        }
      }
    }
    else
    {
      if(is.null(weighted))
        weighted <- TRUE
      else
      {
        if(!weighted)
        {
          weighted <- TRUE
          warning(cat("The network is weighted\n"))
        }
      }
    }
    edg    = matrix(ncol=3)  # For weighted network
    edg.un = matrix(ncol=2)  # For unweighted network
    nr = nrow(adjacency)
    for(i in 1:nr)
    {
      for(j in 1:nr)
      {
        if(adjacency[i,j])
        {
          if(weighted)
            edg = rbind(edg, c(i, j, adjacency[i,j]))
          else
            edg.un = rbind(edg.un, c(i,j))
        }
      }
    }
    if(weighted)
    {
      edg <- edg[-1,]
      colnames(edg) <- c("source", "target", "weight")
    }
    else
    {
      edg.un <- edg.un[-1,]
      colnames(edg.un) <- c("source", "target")
      edg <- edg.un
    }
    
    #attributes(edg)$type <- paste()
    return(edg)
  }
  
  else
    stop("Not an adjacency matrix\n")
}
