% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq.lattice.R
\name{eq.lattice}
\alias{eq.lattice}
\title{Create equivalent lattice network}
\usage{
eq.lattice(adjacency, directed = FALSE, ntry = NULL, measure = "am")
}
\arguments{
\item{adjacency}{An adjacency matrix.}

\item{directed}{Logical scalar. If FALSE (the default) the network is considered as directed,
otherwise undirected.}

\item{ntry}{Number of trials to swap edges. If NULL, the number will be 4 times of the number
of edges.}

\item{measure}{The method used to measure the value of triplet in a network.

"am" implies the arithmetic mean method (default).

"gm" implies the geometric mean method.

"mi" implies the minimum method.

"ma" implies the maximum method.

"bi" implies the binary measure.}
}
\value{
Equivalent lattice network of the specified network in the form of adjacency matrix.
}
\description{
This function generates the equivalent lattice for a given weighted network.
}
\examples{
net <- rbind(c(1,2,4),
c(1,3,2),
c(1,5,3),
c(2,1,4),
c(2,4,1),
c(2,6,1),
c(3,1,2),
c(3,2,4),
c(4,2,1),
c(4,5,2),
c(5,2,2),
c(6,5,1))
adjacency <- edgelist.to.adjacency(net)
lattice <- eq.lattice(adjacency, directed = TRUE)
clustering_w(adjacency)
clustering_w(lattice)
}

