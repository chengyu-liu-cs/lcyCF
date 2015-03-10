lcy.gene.exprs.status <- 
function(genes=NULL, data, group=NULL,fc=c(-1,1),rm.missing=TRUE){

    if(is.null(group)){
        group <- colnames(data)
    }else{
        group <- intersect(group,colnames(data))
    }
    if(is.null(genes)){
        genes   <- rownames(data)
    }else{
        genes   <- intersect(genes, rownames(data))
    }
    if(length(fc) != 2){
        stop('fc should have two values for down and up boundaries, respectively.')
    }
    genes2                  <- intersect(genes, rownames(data))
    regulation              <- matrix(rep(0,2*length(genes2)),nc=2)
    rownames(regulation)    <- genes2
    colnames(regulation)    <- c('UP','DOWN')
    regulation[,'UP']       <- rowSums(data[genes2,group] >= fc[2],na.rm=TRUE)
    regulation[,'DOWN']     <- rowSums(data[genes2,group] <= fc[1],na.rm=TRUE)
    regulation              <- regulation/length(group)
    if(rm.missing==FALSE){
        diff.genes          <- setdiff(genes, rownames(data))
        diff                <- matrix(NA,nc=2,nr=length(diff.genes))
        rownames(diff)      <- diff.genes
        regulation          <- rbind(regulation,diff)
    }
    return(regulation)
}
