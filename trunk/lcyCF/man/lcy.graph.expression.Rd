\name{lcy.graph.expression}
\alias{lcy.graph.expression}
\title{
    overlay gene expression status onto graph showing gene activities on the network level. If there were protein protein interaction, they will be removed.
}
\description{
    original graph does not contains nodes(gene) activity status. By overlying gene activity status on the network, we could see the regulation cascades. which gene are over-/under-expressed or not neutrally expressed.
}
\usage{
lcy.graph.expression(graph, data, up = 2, down = -2, weight=NULL, edge.label=NULL, neutral=FALSE)
}
\arguments{
  \item{graph}{
    an i graph which must contain 'name' attribute for vertex, and the values of 'name' attribute should be the same as rownames of data (at least there should be overlap).
    When weight is not provided, graph must contain an edge attribute which is specified by edge.label. 
}
  \item{activity}{
    a matrix or data frame which have at least two columns. If there are more than two columns, beside the first two columns, the rest of them are ignored. The first column should be the names corresponding to the nodes in the graph and the second column should be numeric value showing the expression (like fold change, expression status (-1, 1, 0) etc. )
}
  \item{up}{
    lower bound for up regulated genes
}
  \item{down}{
    upper bound for down regulated genes
}
  \item{weight}{
    a data matrix or data frame where the first column is values specified by edge attribute 'edgel.label', and the second column is weight (such as 1, 0, -1). weight is valid only if neutral is FALSE.
}
  \item{edge.label}{
    a string specify which edge attribute is used to weight, when weight is NULL.edge.label
}
  \item{neutral}{
    a boolean value used to indicate whether neutrally expressed genes are included in the final graph. When it is set to TRUE, weight are ignored.
}
  \item{g.name}{
    a character string used to specify the name of graph. It is useful when a numeric samplewise network was generated. it is better to have a track.
}
}
\details{
}
\value{
    an igraph which contains one more vertex 'color'. Vertex color green, red and gray indicate over-, under- and neutral expression, respectively. Note neutral expression is valid only if neutral is set to be true.
}
\author{
    Chengyu Liu <Chengyu.liu@helsinki.fi>
}
\keyword{graph }
\keyword{expression }
