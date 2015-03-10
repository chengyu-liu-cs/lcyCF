\name{lcy.DEG.summary}
\alias{lcy.DEG.summary}
\title{
    Statistics on expression matrix.
}
\description{
    Compute p-value(t-test), fold change(ratio), or individualized fold change.
}
\usage{
    lcy.DEG.summary(data, group = list(treat = c(), ctrl = c()), stat.type = c("pvalue", "fc", "both", "individual"),file=NULL, pairwise=FALSE,...)
}
\arguments{
  \item{data}{ A numeric matrix with colnames. The expression matrix should be log transformed if apply fold change (probably add an option showing the data matrix is log based or not.). 
}
  \item{group}{ A list with two attributes, treat and ctrl, showing the groups used to apply ttest or compute fold change.
}
  \item{stat.type}{ a string. One of "pvalue", "fc", "both" and "individual". "pvalue": only ttest are applied; "fc": median value of two groups compared; "both": both pvalue and fold change are computed; "individual": compute fold change for each sample of gene.
}
  \item{file}{ a output filename, default is NULL
}
  \item{pairwise}{ set to be true if the ctrl and treat samples are from the same patient and do pairwise fold change calculation. It ignores the stat.type. It also compute overall fold change and applies statistical test.
}
  \item{\dots}{ for parameter options for ttest.
}
}
\value{
    A matrix. possible colnames could be 'pvalue','fc' or/and 'file name'. Fold change values are log2 transformed.
}

\author{
    Chengyu Liu \email{chengyu.liu@helsinki.fi}
}
\seealso{
    See Also as \code{ttest},  \code{lcy.affy.rma}
}
\keyword{ pvalue }
\keyword{ ttest }
\keyword{ fold change }
\keyword{ individual }
