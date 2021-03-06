% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edgelist.to.adjacency.R
\name{edgelist.to.adjacency}
\alias{edgelist.to.adjacency}
\title{Convert edge list into adjacency matrix}
\usage{
edgelist.to.adjacency(edgelist)
}
\arguments{
\item{edgelist}{The input edge list.}
}
\value{
Returns an adjacency matrix of the specified network.
}
\description{
This funciton converts a weighted or unweighted edge list into an adjacency matrix.
}
\examples{
edg <- rbind(c(1,2,4),
c(1,3,2),
c(1,5,3),
c(2,1,4),
c(2,3,4),
c(2,4,1),
c(2,5,2),
c(2,6,1),
c(3,1,2),
c(3,2,4),
c(3,5,3),
c(4,2,1),
c(4,5,2),
c(5,2,2),
c(5,6,1),
c(6,5,1))
edgelist.to.adjacency(edg)
}

