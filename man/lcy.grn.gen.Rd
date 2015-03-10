\name{lcy.grn.gen}
\alias{lcy.grn.gen}
\title{
    gene regulatory network construction
}
\description{
    Alternative way to generate gene regulatory network. This function does not need to install Moksiskaan. However, the database used in this function is simplified version of moksiskaan database. Only regulation interactions are included. it neither contains pathway information or protein protein interactions.
}
\usage{
lcy.grn.gen(genes, activity=NULL,gap = 0, mode = c("all", "down", "up"), rm.singleton = TRUE, up.bound = 1, low.bound = -1)
}
\arguments{
  \item{genes}{ a vector of genes or a matrix of genes with their activities. it has to be ensembl id. When it is matrix, the first column is gene ids and the second are gene activities. 
}
  \item{gap}{ an integer value that specifies how many gap genes are allowed between querying genes. default (0). Only interactions between querying gene are searched. 
}
  \item{activity}{ used if it is not null (default) and has more than two columns.It is used to overlay gene expression status onto the network. If there are more than two columns, only the first two columns are used. The first column is gene name and second is activity of them.
}
  \item{mode}{ one of "all"(default), "down", "up". down (up) means only down (up) stream genes are searched. All means both up- and down-stream are searched.
}
  \item{rm.singleton}{ default true, indicating singletons in the queried network are removed.
}
  \item{up.bound}{ only used when genes are matrix. bound for upregulated genes
}
  \item{low.bound}{ only used when genes are matrix. bound for downregulated genes
}
}
\value{
    an igraph network
}
\author{
}
\keyword{ moksiskaan }
\keyword{ regulatory }
