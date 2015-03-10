\name{lcy.ihc.grouping}
\alias{lcy.ihc.grouping}
\title{
    breast cancer classification
}
\description{
    classify breast cancer samples based on IHC.
}
\usage{
    lcy.ihc.grouping(data, col.er = "ER", col.pr = "PR", col.her2 = "HER2", val.pos = "Positive", val.neg = "Negative", file='brcaIHC.csv' )
}
\arguments{
  \item{data}{ clinical data matrix/data.frame
}
  \item{col.er}{ colname/index of ER
}
  \item{col.pr}{ colname/index of PR
}
  \item{col.her2}{colname/index of HER2
}
  \item{val.pos}{ value of positive
}
  \item{val.neg}{ value of negative
}
  \item{file}{ output file
}
}
\value{ a list consisting of 4 components, tnbc,her2,lum1,lum2, tpbc
}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}

\keyword{ ihc }
