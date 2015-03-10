\name{lcy.graph.venndiagram}
\alias{lcy.graph.venndiagram}
\title{
    graph venndiagram
}
\description{
    plot graph venndiagram and return igraph objects for each combinations.
}
\usage{
lcy.graph.venndiagram(g.list, v.attr = "", e.attr = "", type = "ellipses", doWeights = F, doEuler = F, colourAlgorithm = "sequential", venn.out = "vennDiagram.pdf")
}
\arguments{
  \item{g.list}{ a list of igraph
}
  \item{v.attr}{ comma separated vertex attribute names
}
  \item{e.attr}{ comma separated edge attribute names
}
  \item{type}{ type of venndiagram. see compute.Venn in Vennerable package. 
} 
  \item{doWeights}{ weight venndiagram, see compute.Venn in Vennerable package.
}
  \item{doEuler}{ see see compute.Venn in Vennerable package.
}
  \item{colourAlgorithm}{ color venndiagram. see VennThemes in in Vennerable package.
}
  \item{venn.out}{ output file name.
}
}
\details{
    let see
}
\value{ return a list of igraphs for each combinations
}
\author{
    chengyu.liu@helsinki.fi
}
\keyword{ graph }
\keyword{ venndiagram }
