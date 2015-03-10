\name{lcy.graph.centrality.sum}
\alias{lcy.graph.centrality.sum}
\title{
    graph centrality summary
}
\description{
}
\usage{
lcy.graph.centrality.sum(graph, mode = "total", directed = TRUE, weights = NULL, scale = TRUE, normalized = FALSE)
}
\arguments{
  \item{graph}{
    an igraph
}
  \item{mode}{ used for computing degree and closeness. it can be one of "all", "out", "in", "total"
}
  \item{directed}{
    logic showing directed graph or undirected graph
}
  \item{weights}{ used by betweenness, closeness or evcent(eigenvector).
}
  \item{scale}{ used by evcent see evcent.
}
  \item{normalized}{ used by degree, betweenness and closeness.
}
}
\details{
    see degree, betweenness, closeness and evcent for detail
}
\value{
    an igraph with additional attributes including degree, betweenness, closeness
}
\author{
    chengyu.liu@helsinki.fi
}
\keyword{ graph }
\keyword{ centrality }
