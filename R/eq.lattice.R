#' Create equivalent lattice network
#' 
#' This function generates the equivalent lattice for a given weighted network.
#' @param adjacency An adjacency matrix.
#' @param directed Logical scalar. If FALSE (the default) the network is considered as directed,
#' otherwise undirected.
#' @param ntry Number of trials to swap edges. If NULL, the number will be 4 times of the number
#' of edges.
#' @param measure The method used to measure the value of triplet in a network.
#' 
#' "am" implies the arithmetic mean method (default).
#' 
#' "gm" implies the geometric mean method.
#' 
#' "mi" implies the minimum method.
#' 
#' "ma" implies the maximum method.
#' 
#' "bi" implies the binary measure.
#' @return Equivalent lattice network of the specified network in the form of adjacency matrix.
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
#' adjacency <- edgelist.to.adjacency(net)
#' lattice <- eq.lattice(adjacency, directed = TRUE)
#' clustering_w(adjacency)
#' clustering_w(lattice)

eq.lattice <- function(adjacency, directed = FALSE, ntry = NULL, measure = "am")
{
  lattice = adjacency
  index = which(lattice!=0, arr.ind=T)  # Indecies of non zero cells
  if(!directed)
  {
    index = index[(which(index[,1]<index[,2])),]
    len = nrow(index)   # Number of edges (undirected)
    i_latt = index[,1]  # row indeces
    j_latt = index[,2]  # column indeces
    if(is.null(ntry))  ntry = 4 * len
    for(k in 1:ntry)
    {
      e1 = 1 + floor(len*runif(1))  # Randomly pick an edge 
      e2 = 1 + floor(len*runif(1))  # Randomly pick another edge
      v1 = i_latt[e1]
      v2 = j_latt[e1]
      v3 = i_latt[e2]
      v4 = j_latt[e2]
      if((v1!=v3) && (v1!=v4) && (v2!=v4) && (v2!=v3))
      {
        if(lattice[v1,v3]==0 && lattice[v2,v4]==0)
        {
          latt_orig = lattice
          eg_latt_orig = adjacency.to.edgelist(latt_orig)
          cc_orig = clustering_w(eg_latt_orig, measure = measure)
          lattice[v1,v3] = lattice[v3,v1] = 1
          lattice[v2,v4] = lattice[v4,v2] = 1
          lattice[v1,v2] = lattice[v2,v1] = 0
          lattice[v3,v4] = lattice[v4,v3] = 0
          eg_latt = adjacency.to.edgelist(lattice)
          cc = clustering_w(eg_latt, measure = measure)
          if(cc > cc_orig)
          {
            i_latt[e1] = v1
            j_latt[e1] = v3
            i_latt[e2] = v2
            j_latt[e2] = v4
          }
          else
          {
            lattice = latt_orig
            v5 = v3
            v3 = v4
            v4 = v5
            if(lattice[v1,v3]==0 && lattice[v2,v4]==0)
            {      
              latt_orig = lattice
              eg_latt_orig = adjacency.to.edgelist(latt_orig)
              cc_orig = clustering_w(eg_latt_orig, measure=measure)
              lattice[v1,v3] = lattice[v3,v1] = 1
              lattice[v2,v4] = lattice[v4,v2] = 1
              lattice[v1,v2] = lattice[v2,v1] = 0
              lattice[v3,v4] = lattice[v4,v3] = 0         
              eg_latt = adjacency.to.edgelist(lattice)
              cc = clustering_w(eg_latt, measure=measure)
              if(cc > cc_orig)
              {
                i_latt[e1] = v1
                j_latt[e1] = v3
                i_latt[e2] = v2
                j_latt[e2] = v4
              }
              else
                lattice = latt_orig
            }
          }
        }
      }
    }
    return(lattice)
  }
  else
  {
    len = nrow(index)   # Number of edges
    i_latt = index[,1]  # row indeces
    j_latt = index[,2]  # column indeces
    if(is.null(ntry))  ntry = 4 * len
    for(k in 1:ntry)
    {
      e1 = 1 + floor(len*runif(1))  # Randomly pick an edge 
      e2 = 1 + floor(len*runif(1))  # Randomly pick another edge
      v1 = i_latt[e1]
      v2 = j_latt[e1]
      v3 = i_latt[e2]
      v4 = j_latt[e2]
      if((v1!=v3) && (v1!=v4) && (v2!=v4) && (v2!=v3))
      {
        if(lattice[v1,v4]==0 && lattice[v3,v2]==0)
        {
          latt_orig = lattice
          eg_latt_orig = adjacency.to.edgelist(latt_orig)
          cc_orig = clustering_w(eg_latt_orig, measure=measure)
          lattice[v1,v4] = lattice[v1,v2]
          lattice[v3,v2] = lattice[v3,v4]
          lattice[v1,v2] = lattice[v3,v4] = 0
          eg_latt = adjacency.to.edgelist(lattice)
          cc = clustering_w(eg_latt, measure=measure)
          if(cc > cc_orig)
          {
            i_latt[e1] = v1
            j_latt[e1] = v4
            i_latt[e2] = v3
            j_latt[e2] = v2
          }
          else
          {
            lattice = latt_orig
          }
        }
      }
    }
    return(lattice)
  }
}
