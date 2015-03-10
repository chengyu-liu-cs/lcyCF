\name{lcy.affy.normalization}
\alias{lcy.affy.normalization}
\title{
    Read affymetrix data.
}
\description{
    Read affymetrix microarray dataset. It uses ReadAffy and rma/mas5 functions.
}
\usage{
    lcy.affy.normalization(file.names = character(0), celfile.path = NULL, norm.method='rma', write.file = NULL)
    
}
\arguments{
  \item{file.names}{ A vector of affy file names to be read. if not given, all files extended with cel in "celfile.path" are read.
}
  \item{celfile.path}{ A path where cel file located. it could be missing as well. But either of "file.name" or "celfile.path" should be provided.
}
  \item{norm.method}{ which method to use to normalize data. rma, mas5
}
  \item{write.file}{ A file name where to write expression data
}
}
\value{
    A matrix, row is genes, colname is file names
}
\author{
    Chengyu Liu \email{chengyu.liu@helsinki.fi}
}
\seealso{
    See Also as \code{\link{lcy.DEG.summary}}
}
\keyword{ affy }
\keyword{ expression}
