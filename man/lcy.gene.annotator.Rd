\name{lcy.gene.annotator}
\alias{lcy.gene.annotator}
\title{
    gene annotation or id conversion.
}
\description{
    Used to annotate genes, mainly for id conversion including id to symbols
}
\usage{
lcy.gene.annotator(keys, columns, keytype)
}
\arguments{
  \item{keys}{
    a vector of id strings, such as ENSEMBL id, ENTREZ id etc.
}
  \item{columns}{
    comma separated string. targets of conversion is separated by comma. "ENSEMBL,ENTREZID,UNIPROT". Supported targets:
    columns(TxDb.Hsapiens.UCSC.hg19.knownGene):
    \describe{
        \item{GENEID}{ Entrez id}
        \item{TXID}{ Transcript ids}
        \item{TXNAME}{Transcript names}
        \item{TXCHROM}{Chromosome of transcript}
        \item{TXSTRAND}{Strand of transcript}
        \item{TXSTART}{Start of transcript}
        \item{TXEND}{End of transcript}
        \item{CDSID}{Coding DNA sequence}
        \item{CDSNAME}{Coding DNA sequence name}
        \item{CDSCHROM}{Chromosome of coding DNA sequence}
        \item{CDSSTRAND}{Strand of coding DNA sequence}
        \item{CDSSTART}{Start of of coding DNA sequence}
        \item{CDSEND}{End of coding DNA sequence}
        \item{EXONID}{Exon ids}
        \item{EXONNAME}{Exon names}
        \item{EXONCHROM}{ Chromosome of exon}
        \item{EXONSTRAND}{Strand of exon}
        \item{EXONSTART}{Start of exon}
        \item{EXONEND}{End of exon}
        \item{EXONRANK}{}
    }
    columns(org.Hs.eg.db):
    \describe{
        \item{ENTREZID}{ entrez id}
        \item{CHR}{ chromosome}
        \item{CHRLOC}{ chromosomal start site location.  Chromosomal locations on the antisense strand have a leading "-" sign (e. g. -1234567)}
        \item{CHRLOCEND}{ chromosomal ending location. Chromosomal locations on the antisense strand have a leading "-" sign (e. g. -1234567)}
        \item{ENZYME}{Enzyme Commission numbers are assigned by the Nomenclature Committee of the International Union of Biochemistry and Molecular Biology}
        \item{MAP}{}
        \item{PATH}{KeGG pathway id (e.g. 04610, 00232 which map to hsa04610 and hsa00232)}
        \item{PMID}{}
        \item{REFSEQ}{RefSeq accessions}
        \item{SYMBOL}{Gene symbol}
        \item{UNIGENE}{Unigene identifier}
        \item{ENSEMBL}{Ensembl gene identifier}
        \item{ENSEMBLPROT}{Ensembl protein identifier}
        \item{ENSEMBLTRANS}{Ensembl transcript identifer}
        \item{GENENAME}{Gene name}
        \item{UNIPROT}{Uniprot identifer}
        \item{GO}{Go id}
        \item{EVIDENCE}{}
        \item{ONTOLOGY}{}
        \item{GOALL}{}
        \item{EVIDENCEALL}{}
        \item{ONTOLOGYALL}{}
        \item{OMIM}{ OMIM identifer} 
        \item{UCSCKG}{UCSC "Known Gene" accession }
        \item{PFAM}{}
        \item{IPI}{}
        \item{PROSITE}{}
        \item{ACCNUM}{}
        \item{ALIAS}{}
    }
}
  \item{keytype}{
    a string indicating the type of input (keys). Supported keytypes:
    keytypes(TxDb.Hsapiens.UCSC.hg19.knownGene)
    \describe{
        \item{GENEID}{ Entrez id}
        \item{TXID}{ Transcript ids}
        \item{TXNAME}{Transcript names}
        \item{EXONID}{Exon ids}
        \item{EXONNAME}{Exon names}
        \item{CDSID}{Coding DNA sequence}
        \item{CDSNAME}{Coding DNA sequence name}
    }
    keys(org.Hs.eg.db):
    \describe{
        \item{ENTREZID}{ entrez id}
        \item{CHR}{ chromosome}
        \item{CHRLOC}{ chromosomal start site location.  Chromosomal locations on the antisense strand have a leading "-" sign (e. g. -1234567)}
        \item{CHRLOCEND}{ chromosomal ending location. Chromosomal locations on the antisense strand have a leading "-" sign (e. g. -1234567)}
        \item{ENZYME}{Enzyme Commission numbers are assigned by the Nomenclature Committee of the International Union of Biochemistry and Molecular Biology}
        \item{MAP}{}
        \item{PATH}{KeGG pathway id (e.g. 04610, 00232 which map to hsa04610 and hsa00232)}
        \item{PMID}{}
        \item{REFSEQ}{RefSeq accessions}
        \item{SYMBOL}{Gene symbol}
        \item{UNIGENE}{Unigene identifier}
        \item{ENSEMBL}{Ensembl gene identifier}
        \item{ENSEMBLPROT}{Ensembl protein identifier}
        \item{ENSEMBLTRANS}{Ensembl transcript identifer}
        \item{GENENAME}{Gene name}
        \item{UNIPROT}{Uniprot identifer}
        \item{GO}{Go id}
        \item{EVIDENCE}{}
        \item{ONTOLOGY}{}
        \item{GOALL}{}
        \item{EVIDENCEALL}{}
        \item{ONTOLOGYALL}{}
        \item{OMIM}{ OMIM identifer} 
        \item{UCSCKG}{UCSC "Known Gene" accession }
        \item{PFAM}{}
        \item{IPI}{}
        \item{PROSITE}{}
        \item{ACCNUM}{}
        \item{ALIAS}{}
    }
}
}
\details{
    org.Hs.eg.db package was required. It is possible there are multiple mappings and NA mappings. 
    >orgSymbols <- keys(org.Hs.eg.db, keytype="SYMBOL")
    >#extract all the transcript ids
    >txid = keys(TxDb.Hsapiens.UCSC.hg19.knownGene, "TXID")
}
\value{
    returns a matrix where rows are query and columns are targets of conversion.
}
\author{
    Chengyu Liu <Chengyu.liu@helsinki.fi>
}
\keyword{ gene }
\keyword{ annotation }
\keyword{ annotator }
