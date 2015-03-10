\name{lcy.graph.search}
\alias{lcy.graph.search}
\title{
    search subgraph
}
\description{ search downstream or upstream of given a graph and v.attribute. It is not the same with induced subgraphs. 
}
\usage{
lcy.graph.search(g, v.attr = list(), e.attr = list(), e.color, mode = c("all", "down", "up"), gap = 0, rm.singleton=TRUE)
}
\arguments{
  \item{g}{ an igraph
}
  \item{rm.singleton}{ default(TRUE) remove singleton in the graph 
}
  \item{v.attr}{ a list of vertex attributes. the list should be named with vertex attributes.
}
  \item{e.attr}{ a list of edge attributes. the list should be named with edge attributes
}
  \item{e.color}{ a named edge color vector. The name must be the name of edge type which are the values of 'linkType' edge attribute.
}
  \item{mode}{ one of three strings (all, down, up), refering to all the connection, downstream connection and upstream connection.
}
  \item{gap}{ whether a gap should be included when the subgraph is induced.
}
}
\details{
    lets see
}
\value{
    an igraph object with an additional edge attribute 'color' which shows different color for different edge types if 'linkType' edge attribute exists. 
}
\author{
    chengyu.liu@helsinki.fi
}
\examples{
    g <- lcy.graph.search(g=moksiskaan,v.attr=list(name=V(moksiskaan)$name[1:10]),e.attr=list(linkType=c('gene expression','gene repression','protein-protein interaction')))
}
\keyword{ search}
\keyword{ graph }
