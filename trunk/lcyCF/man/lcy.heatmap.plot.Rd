\name{lcy.heatmap.plot}
\alias{lcy.heatmap.plot}
\title{
    plot heatmap
}
\description{
    plot heatmap using heatmap.2 function. It is possible to add row and column color bars based one given labels.
}
\usage{
    lcy.heatmap.plot(data, output = "heatmap.pdf", col = "greenred", group = NULL, symbreaks = TRUE, density.info = "none", cexRow = 0.5, cexCol = 0.3, width = 16.5, height = 12, keysize = 0.5, ...)
}
\arguments{
  \item{data}{ numeric matrix.
}
  \item{output}{ output file name
}
  \item{col}{ a string, one of redgreen, greenred, bluered, redblue,topo.colors(75),topo.colors(100),redgreen(75),heat.colors
}
  \item{scale}{ scale='row'/'col'/'non'(default),
}
  \item{hclustfun}{ hclustfun =function(y) hclust(y,method=x),
}
  \item{group}{ a list of two components col and row, each component have two columns. The first column is labels which are identical with colnames/rownames of data, the second is labels for the row/columns
}
  \item{symbreaks}{ logic value to indicate whether key are broken symmetically.
}
  \item{density.info}{ density infor in key
}
  \item{cexRow}{ adjust label size of row
}
  \item{cexCol}{ adjust label size of column
}
  \item{width}{ width of output pdf
}
  \item{height}{ height of output pdf
}
  \item{keysize}{ keysize
}
  \item{\dots}{ more parameter for heatmap.2
}
}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}
\keyword{ heatmap }
