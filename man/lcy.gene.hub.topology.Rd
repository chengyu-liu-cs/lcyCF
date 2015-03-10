\name{lcy.gene.rank.topology}
\alias{lcy.gene.rank.topology}
\title{
   topology based gene expression representation 
}
\description{
    take gene regualtion topology into account to rank genes based on their regulating genes
}
\usage{
lcy.gene.rank.topology(g, exprs)
}
\arguments{
  \item{g}{ an igraph object which have 'name' vertex attribute. this is should identical part of rownames of exprs.
}
  \item{exprs}{ a matrix of 1 or n columns. each column is one sample. rownames should be the same type of V(g)$name
}
}
\details{ it takes gene regulation topology into account. sum of Es + Et * Et. Es/Et are expression of source/target genes. Et: the type of regulation(1 activation, -1 inhibition)
}
\value{ return a matrix.
    
}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}
\keyword{ gene }
\keyword{ transform }
