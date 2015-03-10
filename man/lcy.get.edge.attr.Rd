\name{lcy.get.graph.attr}
\alias{lcy.get.graph.attr}
\title{
    get graph attributes
}
\description{
    get graph attributes of a igraph object. get.graph.attribute in igraph can only get one attribute each time. it accepts comma separated attribute name and return a matrix, each column for each attribute
}
\usage{
lcy.get.graph.attr(g, attr)
}
\arguments{
  \item{g}{ an igraph object
}
  \item{attr}{ a comma separated strings. it also can be '*' which means all the edge attributes in the igraph
}
}
\value{
    return a matrix of characters.
}
\author{
    chengyu.liu@helsinki.fi
}
\keyword{ attribute}
