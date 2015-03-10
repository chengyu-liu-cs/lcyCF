\name{lcy.graph.union}
\alias{lcy.graph.union}
\title{
    graph union
}
\description{
    find union graph with combination of edge attribute. The edges with same nodes but with different edge labels are considered as different edges.If there are empty igraph objects, they will be removed.
}
\usage{
lcy.graph.union(g.list, e.attr = "", singleton = TRUE, weighting=TRUE)
}
\arguments{
  \item{g.list}{ a list of igraphs. the graph should be the same type, all of them are directed or undirected.
}
  \item{e.attr}{ a comma separated edge attributes.
}
  \item{singleton}{ whether singleton should be included or not.
}
  \item{weighting}{ decide whether edge should be weighted based on frequency of edges. Default is true.
}
}
\details{
    lets see whether should mention
}
\value{
    return an igraph object which is union of graphs and edge attributes which are listed in edge.attr 
}
\author{
    chengyu.liu@helsinki.fi 
}
\keyword{ graph }
\keyword{ union }
