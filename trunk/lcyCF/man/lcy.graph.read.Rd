\name{lcy.graph.read}
\alias{lcy.graph.read}
\title{
    read graphs
}
\description{
    read (all) graphs from a directory. it can remove attributes of vertex,edge or graph when graphs are being read.
}
\usage{
    lcy.graph.read(path='.', file.list=NULL, format='graphml',vertex.name, vertex.label,vertex.attr.rm, edge.label, edge.attr.rm, graph.attr.rm)
}
\arguments{
  \item{path}{ string where *.graphml files locate
}
  \item{file.list}{ a vector of strings indicating which files are read
}
  \item{format}{ a string from 'graphml', 'gml', 'edgelist'. Later extended to pajek ....
}
  \item{vertex.name}{ a string indicating name of nodes. it should be unique, is used to distinguish different nodes.
}
  \item{vertex.label}{ a string indicating label of nodes, which is used to label vertex in the plot.
}
  \item{vertex.attr.rm}{ string vector, the attributes that should be removed from vertex attributes  
}
  \item{edge.label}{ a string indicating label of edges
}
  \item{edge.attr.rm}{ string vector, the attributes that should be removed from edge attributes  
}
  \item{graph.attr.rm}{ string vector, the attributes that should be removed from graph attributes  
}
}
\details{
    Read files extended with xml. When file.list is given, only these files are read from the pathname. 
}
\value{
    A list of igraph objects with name attribute.
}
\author{
    Chengyu Liu \email{chengyu.liu@helsinki.fi}
}

\keyword{ read }
\keyword{ graph }
