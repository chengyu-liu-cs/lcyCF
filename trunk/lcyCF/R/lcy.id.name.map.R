lcy.id.name.map <- function(source,type='GeneID', target='GeneName',rnaType='protein_coding,miRNA',rm.empty=FALSE,rm.duplication=FALSE){
    # load id <-> name mapping table, LCY.map.data, LCY.map.gene.type, LCY.map.supported.attributes 
    #load('/storageBig/storageBig1/czliu/reloadableData/geneID_geneName_mapping.RData')
    data(lcyCF)
    if(missing(source))
        stop('source could NOT be missing.')
    stopifnot(length(type)==1, is.character(type), as.logical(lcy.in(type,list=names(LCY.map.supported.attributes))[,1]))
    stopifnot(length(target)==1, is.character(target))

    target          <- strsplit(target,',')[[1]]
    stopifnot(sum(lcy.in(target, list=names(LCY.map.supported.attributes))[,1]) == length(target))
    target.col      <- LCY.map.supported.attributes[target]
    names(target.col)   <- NULL
    stopifnot(length(rnaType)==1, is.character(rnaType))
    
    if(rnaType != 'any'){
        rnaType <- strsplit(rnaType,',')[[1]]
        tmp <- c()
        for(i in rnaType){
            tmp <- rbind(tmp,LCY.map.data[LCY.map.data[,LCY.map.supported.attributes['BioType']] == i,])
        }
        LCY.map.data <- tmp
    }
    if(rm.duplication == TRUE)
        source              <- unique(source)
    len.source          <- length(source)
    len.target          <- length(target)
    results             <- matrix(rep('',len.source * (len.target+1)),nc=(len.target+1))
    colnames(results)   <- c(type,target)
    # GeneName can map to different GeneIDs. So use multiple lines to show the results.
    for (i in 1: length(source)){
        index           <- source[i] == LCY.map.data[,LCY.map.supported.attributes[type]]
        if(sum(index)> 1){
            results[i,] <- c(source[i],apply(LCY.map.data[index,target.col,drop=FALSE],2,function(x){ paste(unique(x),collapse=',')}))
        }else if(sum(index) == 1){
            results[i,] <- c(source[i],LCY.map.data[index,target.col,drop=FALSE])
        }
    }
    if(rm.empty == TRUE){
        index       <- results[,1] == ""
        if(sum(index) != 0)
            results     <- results[-which(index),]
    }
    results
}

