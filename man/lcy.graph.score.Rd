\name{lcy.graph.score}
\alias{lcy.graph.score}
\title{
    lcy.graph.score
}
\description{
    used to compute score of a graph using fold change matrix. 
}
\usage{
lcy.graph.score(g, data, edge.label, edge.weight)
}
\arguments{
  \item{g}{ a igraph object. it could be directed or undirected.
}
  \item{data}{ expression or fold change matrix
}
  \item{edge.label}{ the attribute name of edge (edge type) which corresponds to edge.weight.
}
  \item{edge.weight}{ used to decide edge weights. default: -1 = gene repression, 1= gene expression 
}
}
\details{
    if the graph is directed edge.label and edge.weight are taken into account. otherwise they are ignored. Edge.weight is used to take the graph structure into account when computing score. For instance, gene repression use -1 and gene expression use 1.
}
\value{
    a double value
}
\author{
    Chengyu Liu<Chengyu.liu@helsinki.fi>
}
\examples{
    lcy.graph.score(g,exprs,edge.label='linkType',edge.weight=cbind(c('gene expression','gene repression'),c(1,-1)))
}
\keyword{ score }
\keyword{ graph }
