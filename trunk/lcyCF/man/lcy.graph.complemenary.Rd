\name{lcy.graph.complemenary}
\alias{lcy.graph.complemenary}
\title{
    graph complementary
}
\description{
}
\usage{
lcy.graph.complemenary(g1, g2, v.attr = "", e.attr = "", singleton = TRUE)
}
\arguments{
  \item{g1}{ igraph object
}
  \item{g2}{ igraph object
}
  \item{v.attr}{ a comma separated vertex attributes.
}
  \item{e.attr}{ a comma separated edge attributes.
}
  \item{singleton}{ whether singleton should be included or not.
}
}
\details{
    vertex should have 'name' attribute which is the labels of node. Use 'name' to distinguish different nodes.check whether all given graphs are directed or indirected graphs. They should be the same.
    lets see whether should mention
}
\value{
    return a list of two components, each of which is complement of the other graph.  
}
\author{
    chengyu.liu@helsinki.fi 
}
\keyword{ graph }
\keyword{ complementary }
