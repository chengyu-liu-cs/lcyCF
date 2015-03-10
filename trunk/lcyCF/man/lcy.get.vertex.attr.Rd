\name{lcy.get.vertex.attr}
\alias{lcy.get.vertex.attr}
\title{
    get vertex attributes
}
\description{
    get vertex attributes of a igraph object. get.vertex.attribute in igraph can only get one attribute each time. it accepts comma separated attribute name and return a matrix, each column for each attribute
}
\usage{
lcy.get.vertex.attr(g, attr)
}
\arguments{
  \item{g}{ an igraph object
}
  \item{attr}{ a comma separated strings. it also can be '*' which means all the vertex attributes in the igraph
}
}
\value{
    return a matrix of characters.
}
\author{
    chengyu.liu@helsinki.fi
}
\keyword{ attribute}
\keyword{ vertex }
