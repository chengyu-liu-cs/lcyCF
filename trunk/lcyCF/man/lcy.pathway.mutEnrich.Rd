\name{lcy.pathway.mutEnrich}
\alias{lcy.pathway.mutEnrich}
\title{
    pathway enrichment analysis based on mutation data
}
\description{
    Used to do pathway enrichment anlaysis based on mutation data. duplicated mutation genes are considered as different mutations and are counted when overlap with pathway was checked. Genes only overlapped with the genes in the database are used.
}
\usage{
lcy.pathway.mutEnrich(variant, key.column = "", database = "PID")
}
\arguments{
  \item{variant}{
    a data frame or matrix, the id should uniprot id. Might contain duplicated uniprot ids and considered as different mutation in the same genes.
}
  \item{key.column}{
    column name or index of the key. The column uniprot ids are.
}
  \item{database}{
    there are two pathway databases, pathway interaction database (PID) and reactiome (REACTOME). 
    possible value is PID (default) and REACTOME.
}
  \item{dup.rm}{
    decide whether remove duplicated ids. Defauls is TRUE 
}
}
\details{
}
\value{ 
    a data frame with pathwayId which is used in NCI database, pathwayName,pathwayDesc, totalGene, overlappedGene, fraction and pvalue. pvalue is calculated using fisher's exact test.
}
\author{
    Chengyu Liu <Chengyu.liu@helsinki.fi>
}

\keyword{ pathway }
\keyword{ enrichment }
\keyword{ analysis }
\keyword{ mutation }
