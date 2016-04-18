#' Check whether an object is an edge list
#' 
#' This function checks whether an object is an edge list.
#' @param adjmatrix The input adjacency matrix.
#' @return Logical scalar, whether the input object is an edge list.
#' @export
#' @examples
#' edg <- rbind(c(1,2,4),
#' c(1,3,2),
#' c(2,1,4),
#' c(2,4,1),
#' c(3,1,2),
#' c(3,2,4),
#' c(4,2,1))
#' is.edgelist(edg)

is.edgelist <- function(edgelist)
{
  if(is.matrix(edgelist) || is.data.frame(edgelist))
  {
    if(ncol(edgelist)==2)
    {
      return(TRUE)
    } else 
      if(ncol(edgelist)==3)
      {
        if(is.numeric(edgelist))
        {
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
  } else {
    return(FALSE) }
}