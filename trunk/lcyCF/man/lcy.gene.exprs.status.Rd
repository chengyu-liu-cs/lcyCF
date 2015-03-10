\name{lcy.gene.exprs.status}
\alias{lcy.gene.exprs.status}
\title{
    check gene expression status
}
\description{
    check gene expression status, up or down regulated in terms of the fragment of samples
}
\usage{
    lcy.gene.exprs.status(genes=NULL, data, group = NULL, fc = c(-1, 1), rm.missing = TRUE)
}
\arguments{
  \item{genes}{ a vector of genes. default is NULL. when it is null all the genes in data were used. otherwise intersection between genes and rownames of data would be taken.
}
  \item{data}{ fold change matrix. rownames of data and genes are should be the same type, geneName or geneID.
}
  \item{group}{ a vector of samples, should be the same with colnames of data. if there are differences, intersection would be taken.
}
  \item{fc}{ boundary of differential level. first element is the underexpression threshold, second is over-expression threshold.
}
  \item{rm.missing}{ if TRUE and genes is missing in rownames of data, rm missing genes from results, otherwise put them into the end with NAs
}
}
\value{
    a matrix with two columns, UP and DOWN
}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}
\keyword{ gene expression status }
