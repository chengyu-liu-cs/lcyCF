\name{lcy.DEG.samr}
\alias{lcy.DEG.samr}
\title{
    identify differentially expressed genes
}
\description{
    identify differentially expressed genes using package samr. used in tcga paper. 
}
\usage{
lcy.DEG.samr(x, y, resp.type = "Two class unpaired", min.foldchange = 2, delta = 0.3, nperms = 100, nvals = 100, logged2 = TRUE, censoring.status = NULL, eigengene.number = 1, assay.type = "array", qvalue = 20, outpath = "./", genenames = NULL, ...)
}
\arguments{
  \item{x}{
    numeric matrix, rows are genes, columns are patients. it's better to contain rownames
}
  \item{y}{
    a numeric vector with the same column length of x. it must be 1 and 2 when resp.type = "Two class unpaired". 
}
  \item{resp.type}{
    \describe{
        \item{Quantitative}{ for a continuous parameter}
        \item{Two class unpaired}{it must be 1 and 2}
        \item{Survival}{for censored survival outcome}
        \item{Multiclass}{more than 2 groups}
        \item{ne class}{for a single group}
        \item{Two class paired}{for two classes with paired observations}
        \item{Two class unpaired timecourse}{}
        \item{One class time course}{}
        \item{Two class.paired timecourse}{}
        \item{Pattern discovery}{}
    }
}
  \item{min.foldchange}{
    The minimum fold change desired; should be >1; default is zero, meaning no fold change criterion is applied. Default 2.

}
  \item{delta}{
    Value of delta to define cutoff rule.
}
  \item{nperms}{
    Number of permutations used to estimate false discovery rates.
}
  \item{nvals}{
    Number of delta values used. For array data, the default value is 50. For sequencing data, the value will be chosen automatically.
}
  \item{logged2}{
    Has the data been transformed by log (base 2)? This information is used only for computing fold changes.
}
  \item{censoring.status}{
    n-vector of censoring censoring.status (1= died or event occurred, 0=survived, or event was censored), needed for a censored survival outcome.
}
  \item{eigengene.number}{
    Eigengene to be used (just for resp.type="Pattern discovery")
}
  \item{assay.type}{
    either array or seq sepecifying array and sequencing platforms.
}
  \item{qvalue}{
    cut-off for the significant genes.
}
  \item{outpath}{
    output directory. default current directory.
}
  \item{genenames}{
    optional, specifying genenames.
}
  \item{\dots}{
    see samr manual for details
}
}
\details{
}
\value{
    it returns a data.frame where there are 7 columns. including q-value, pvalue, fold change, foldchange(logged "Numerator(r)"), and f-score.
}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}
\keyword{ sam }
\keyword{ deg }
