lcy.pathway.mutEnrich <- function(variant, key.column='', database='PID',dup.rm=TRUE){
    # variant should be annotated, and uniProt id should be used for gene identifier. 
    # get pathway ids
    pathwayId       <- lcy.pathway.search(columns='uniProt,pathwayId',database=database)$res
    # get gene sets for each pathways
    allgene         <- pathwayId[,1]
    pathwaySet      <- split(pathwayId,f=pathwayId$pathwayId)
    pathwaySet      <- lapply(pathwaySet,function(x) unique(x[,1]))
    # group variants by genes and count the frequencies of variants for each gene
    if(key.column == ''){
        key.column <- 1
    }
    variant         <- variant[variant[,key.column] %in% allgene,,drop=FALSE]
    variant.gene    <- split(variant[,key.column],variant[,key.column])
    names           <- names(variant.gene)
    if(dup.rm){
        freq        <- cbind(freq=rep(0,length(names)), genes=names)
        gene        <- names
    }else{
        len         <- unlist(lapply(variant.gene,function(x){length(x)}))
        freq        <- cbind(freq=len, genes=paste(names, len, sep='__'))
        gene        <- variant[,key.column]
    }
    rownames(freq)  <- names
    ### summarize
    mutLen          <- length(gene)
    enriched        <- lapply(pathwaySet, function(x) {
                            set.len     <- length(x)
                            #index       <- match(gene, x)
                            index       <- gene %in% x
                            in.set      <- sum(index)
                            not.in.set  <- sum(!index)
                            in.gene     <- unique(gene[index])
                            in.gene     <- paste(freq[in.gene,2],collapse=',')
                            fraction    <- in.set/set.len
                            # fisher's exact test. whole genome 19092. mutated/unmutated; geneSet/NotInGeneSet
                            m           <- c(in.set, not.in.set, set.len-in.set, length(allgene)-set.len-not.in.set)
                            pvalue      <- fisher.test(matrix(m,nrow=2,ncol=2,byrow=TRUE),alternative="greater")$p.value
                            return(c(set.len, in.set, fraction, pvalue, in.gene))
                        })
    enriched    <- do.call(rbind, enriched)
    enriched    <- enriched[order(enriched[,4],decreasing=FALSE),]
    pathwayId   <- rownames(enriched[order(enriched[,4],decreasing=FALSE),])

    # annotate pathway with name and description
    tmp <- lapply(pathwayId, function(x) {
        y <- lcy.pathway.search(columns='pathwayId,pathwayName,pathwayDesc',database=database, condition=list(uniProt=NULL,pathwayDesc=NULL,pathwayName=NULL,pathwayId=x))$res
        return(y)
        })
    pathway         <- do.call(rbind,tmp)
    ret             <- cbind(pathway,enriched)
    colnames(ret)   <- c('pathwayId','pathwayName','pathwayDesc','totalGene','overlappedGene','fraction','pvalue','genes')
    return(ret)
}

