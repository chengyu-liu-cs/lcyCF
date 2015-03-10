\name{lcy.graph.intersect}
\alias{lcy.graph.intersect}
\title{
    graph intersection
}
\description{ get graph interaction under condiseration. it is different from graph.intersect in igraph. it considers vertex and edge attributes as well. when vertex or edge attributes different, the edges are not considered as the same.
}
\usage{
lcy.graph.intersect(g.list, v.attr = "", e.attr = "", singleton = TRUE)
}
\arguments{
  \item{g.list}{
    a list of igraph object
}
  \item{v.attr}{ a comma separated vertex attributes.
}
  \item{e.attr}{ a comma separated edge attributes.
}
  \item{singleton}{ whether singleton should be included or not.
}
}
\details{
    lets see whether should mention
}
\value{
    return an igraph object which is intersection of graphs, and vertex andedge attributes which are listed in edge.attr 
}
\author{
    chengyu.liu@helsinki.fi 
}
\keyword{ graph }
\keyword{ intersect }
