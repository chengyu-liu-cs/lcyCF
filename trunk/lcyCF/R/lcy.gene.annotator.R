lcy.gene.annotator <- function(keys, columns, keytype){
    require('org.Hs.eg.db')
    require('TxDb.Hsapiens.UCSC.hg19.knownGene')
    columns <- strsplit(columns,split=',')[[1]]
    # columns(target) in TxDB database, keytypes and columns are different for TxDb database
    keytypes.TxDb.Hsapiens.UCSC.hg19.knownGene  <- columns(TxDb.Hsapiens.UCSC.hg19.knownGene) # gene model(transcript) annotation, such as exon, transcript, start, end, etc.
    # only in TxDb database
    in.TxDb.Hsapiens.UCSC.hg19.knownGene        <- columns[columns %in% keytypes.TxDb.Hsapiens.UCSC.hg19.knownGene]
    # columns in org.Hs database. keytypes and columns are the same for TxDb database
    keytypes.org.Hs.eg.db   <- keytypes(org.Hs.eg.db) # gene annotation
    # only in org.Hs database
    in.org.Hs.eg.db         <- columns[columns %in% keytypes.org.Hs.eg.db]
    if(length(in.org.Hs.eg.db) == 0){
        res1 <- NULL
    }else{
        res1 <- select(org.Hs.eg.db, keys=keys, columns=c('ENTREZID', in.org.Hs.eg.db), keytype=keytype)
    }
    if(length(in.TxDb.Hsapiens.UCSC.hg19.knownGene) == 0){
        res2 <- NULL
    }else{
        res2 <- select(TxDb.Hsapiens.UCSC.hg19.knownGene, keys=res1$ENTREZID, columns=c('GENEID',in.TxDb.Hsapiens.UCSC.hg19.knownGene), keytype='GENEID')
        names(res2) <- c('ENTREZID', in.TxDb.Hsapiens.UCSC.hg19.knownGene)
    }
    if(is.null(res1) & is.null(res2)){
        res <- NULL
    }else{
        if((!is.null(res1)) & (!is.null(res2)))
            res <- merge(res1,res2,'ENTREZID')
        else if(!is.null(res1))
            res <- res1
        else if(!is.null(res2))
            res <- res2
    }
    return(res)
}
