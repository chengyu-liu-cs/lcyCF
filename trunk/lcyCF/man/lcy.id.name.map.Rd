\name{lcy.id.name.map}
\alias{lcy.id.name.map}
\title{
    convert gene identifier to another
}
\description{
    Convert GeneID to other identifer, such GeneName and Desc, or vice versus. It also limits the rnaType, for instance, protein_coding, miRNA,etc.
}
\usage{
    lcy.id.name.map(source, type, target, rnaType, rm.empty = FALSE,rm.duplication=FALSE)
}
\arguments{
    \item{source}{
        a vector of identifiers, such as GeneID or GeneName. Could have duplicated identifiers, but unique values are used.
}
    \item{type}{
        a string showing which type of source key is. It could be GeneID, GeneName,BioType,Decs(in practice, it is not valuable.)
}
    \item{target}{
        a comma separated string, each is one type of identifier or annoation(GeneID,GeneName,BioType,Decs).
}
    \item{rnaType}{
        a string of name of valid rnaType. 
        "miRNA"                    "snoRNA"                  
        "misc_RNA"                 "snRNA"                   
        "rRNA"                     "pseudogene"              
        "lincRNA"                  "antisense"               
        "processed_transcript"     "Mt_tRNA"                 
        "Mt_rRNA"                  "protein_coding"          
        "sense_intronic"           "sense_overlapping"       
        "3prime_overlapping_ncrna" "IG_V_pseudogene"         
        "non_coding"               "IG_C_pseudogene"         
        "TR_V_pseudogene"          "TR_J_pseudogene"         
        "polymorphic_pseudogene"   "IG_J_pseudogene"         
        "TR_V_gene"                "TR_J_gene"               
        "TR_C_gene"                "IG_C_gene"               
        "IG_J_gene"                "IG_V_gene"               
        "TR_D_gene"                "IG_D_gene"               
        "processed_pseudogene"     "LRG_gene"
}
    \item{rm.empty}{
        logic value. TRUE remove empty source key from results.
}
    \item{,rm.duplication=FALSE}{
        logic value. TRUE remove multiple source key from source.
}
}
\value{
    a matrix with targeted annotation in column and source keys in row.
}
\author{
    Chengyu Liu \email{chengyu.liu@helsinki.fi}
}
\keyword{ convert }
\keyword{ map }
