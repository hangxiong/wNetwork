% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strength.dist.plot.R
\name{strength.dist.plot}
\alias{strength.dist.plot}
\title{Plot strength distribution of a network}
\usage{
strength.dist.plot(edgelist, type = "out", by = 1, alpha = 0.5,
  fit.line = TRUE)
}
\arguments{
\item{edgelist}{The input edge list.}

\item{type}{Option indicating whether out- or in-measures to be calculated. The default is "out".
The setting is irrelevant for undirected networks, but it must be specified.}

\item{by}{number: increment of the sequence}

\item{alpha}{Default is 0.5.}
}
\value{
Plot of strength distribution of the specified network.
}
\description{
This function plots the strength distribution of a network in the form of edge list.
}
\examples{
g <- barabasi.game(1000, directed = FALSE)
edg <- get.edgelist(g)
edg = cbind(edg, runif(nrow(edg), 0, 1))
strength.dist(edg)
strength.dist.plot(edg)
}

