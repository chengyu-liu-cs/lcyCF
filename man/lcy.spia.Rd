\name{lcy.spia}
\alias{lcy.spia}
\title{
    pathway enrichment analysis
}
\description{
    pathway enrichment analysis (SPIA, previously known as Pathway-Express)
}
\usage{
lcy.spia(de, all, nB = 2000, organism = "hsa", pathids = NULL, plots = FALSE, verbose = "TRUE", data.dir = NULL, beta = NULL, combine = "fisher", output ='spia.csv')
}
\arguments{
  \item{de}{
  A named vector containing log2 fold-changes of the differentially expressed genes. The names of this numeric vector are Entrez gene IDs.
}
  \item{all}{
  A vector with the Entrez IDs in the reference set. If the
  data was obtained from a microarray experiment, this set will
  contain all genes present on the specific array used for the
  experiment. This vector should contain all names of the ‘de’
  argument.
}
  \item{nB}{
   Number of bootstrap iterations used to compute the P PERT
   value. Should be larger than 100. A recommended value is
   2000.
}
  \item{organism}{
  A three letter character designating the organism. See a full list at ftp://ftp.genome.jp/pub/kegg/xml/organisms.
}
  \item{pathids}{
  A character vector with the names of the pathways to be
  analyzed. If left NULL all pathways available will be tested.
}
  \item{plots}{
  If set to TRUE, the function plots the gene perturbation
  accumulation vs log2 fold change for every gene on each
  pathway. The null distribution of the total net accumulations
  from which PPERT is computed, is plotted as well.  The
  figures are sent to the SPIAPerturbationPlots.pdf file in the
  current directory.
}
  \item{verbose}{
  If set to TRUE, displays the number of pathways already
  analyzed.
}
  \item{data.dir}{
  Location of the "organism"SPIA.RData file containing the
  pathways data generated with makeSPIAdata.  If set to NULL
  will look for this file in the extdata folder of the SPIA
  library.
                                
}
  \item{beta}{
        Weights to be assigned to each type of gene/protein relation
          type. It should be a named numeric vector of length 23, whose
          names must be:
          ‘c("activation","compound","binding/association","expression","inhibition","activation_phosphorylation","phosphorylation",
          "indirect","inhibition_phosphorylation","dephosphorylation_inhibition","dissociation","dephosphorylation","activation_dephosphorylation",
          "state","activation_indirect","inhibition_ubiquination","ubiquination","expression_indirect","indirect_inhibition","repression",
          "binding/association_phosphorylation","dissociation_phosphorylation","indirect_phosphorylation")’

          If set to null, beta will be by default chosen as:
          c(1,0,0,1,-1,1,0,0,-1,-1,0,0,1,0,1,-1,0,1,-1,-1,0,0,0).
}
  \item{combine}{
        Method used to combine the two types of p-values. If set to
          ‘"fisher"’ it will use Fisher's method. If set to ‘"norminv"’
          it will use the normal inversion method.
}
  \item{output}{
        an output file name. if it is not NULL(default), then write to the file.
}
}
\details{
}
\value{
    A data frame containing the ranked pathways and various
     statistics: ‘pSize’ is the number of genes on the pathway; ‘NDE’
     is the number of DE genes per pathway; ‘tA’ is the observed total
     preturbation accumulation in the pathway; ‘pNDE’ is the
     probability to observe at least ‘NDE’ genes on the pathway using a
     hypergeometric model; ‘pPERT’ is the probability to observe a
     total accumulation more extreme than ‘tA’ only by chance; ‘pG’ is
     the p-value obtained by combining ‘pNDE’ and ‘pPERT’; ‘pGFdr’ and
     ‘pGFWER’ are the False Discovery Rate and respectively Bonferroni
     adjusted global p-values; and the ‘Status’ gives the direction in
     which the pathway is perturbed (activated or inhibited).
     ‘KEGGLINK’ gives a web link to the KEGG website that displays the
     pathway image with the differentially expressed genes highlighted
     in red.

}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}
\keyword{ spia }
\keyword{ pathway }
\keyword{ enrichment}
\keyword{ analysis }
