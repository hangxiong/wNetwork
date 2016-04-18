#' Check whether an object is an adjacency matrix
#' 
#' This function checks whether an object is an adjacency matrix.
#' @param adjmatrix The input adjacency matrix.
#' @return Logical scalar, whether the input object is an adjacency matrix.
#' @export
#' @examples
#' adj = rbind(c(0, 1, 5, "a"),
#' c(3, 4, "b", 7),
#' c("abc", 2, 3, 0),
#' c(4, 0.5, 6, 7))
#' is.adjmatrix(adj)

is.adjmatrix <- function(adjmatrix)
{
  if((is.matrix(adjmatrix) || is.data.frame(adjmatrix)) && is.numeric(adjmatrix))
  {
    return(TRUE)
    if(all(adjmatrix %in% c(0,1)))
    {
      cat("Unweighted network")
    } else {
      cat("Weighted network")
    }
  } else {
    return(FALSE) }
}