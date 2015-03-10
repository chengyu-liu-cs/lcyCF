lcy.misc.cutdendrogram <- function(dendrogram, h) {
    # simple function that simply cut the dendrogram and return the cluster members.
    levels  <- cut(dendrogram, h=h)$lower
    group   <-lapply(levels, labels)
    names(group) <- paste('cluster', 1:length(levels),sep='_')
    return(group)
}


lcy.misc.filter.marker <- function (data, label, aucth = 0.9, unique=FALSE){
    ## Used to identify markers which are predictive or specific for a subtype. For each gene, the predictive power is evaluated for each subtype. When unique is FALSE, the retured genes are predictive for at least a subtype (maybe more). NOTE when unique is set to TRUE, it will return genes which are specific for a cluster which means if a gene is significant in two clusters compared with the others, then it will be returned. Since it is NOT specific for a cluster. If label does not have names, then the label order should be the same as the data.
    require(ROCR)
    require(Vennerable)
    sigdat <- data
    if(is.null(rownames(sigdat))){
        rownames(sigdat) <- as.character(1:ncol(sigdat))
    }
    sigGeneIds <- rownames(sigdat)
    compareTwoClus <- function(sigdat, label, clu = 1) {
        temp                <- label
        temp[label == clu] <- 0
        temp[label != clu] <- 1
        if(is.null(names(tmp))){
            names <- 1: length(temp)
        }
        sigdat  <- sigdat[, names]
        preds   <- apply(sigdat, 1, prediction, labels=temp)
        aucs    <- unlist(lapply(preds, function(x) {
                        (performance(x, measure = "auc"))@y.values[[1]]
                    }))  
        aucs    <- rowMax(cbind(aucs, 1-aucs))
        return(rownames(sigdat)[which(aucs >= aucth)])
    }
    sigs <- lapply(unique(label), function(x) {
        sigs <- compareTwoClus(sigdat, label, x)
    })
    if(unique) {
        K       <- length(sigs)
        if(length(sigs) <= 3){
            type = 'circles'
        }else{
            type = 'ellipses'
        }
        venn    <- lcy.venn.diagram(Sets=sigs, type=type)
        index   <- unlist(lapply(rownames(venn), function(x) {sum(strsplit(x,split='_')[[1]] == 'NOT')})) == (K-1)
        sigs  <- lapply(venn[index],function(x){tmp <-strsplit(x,split=',')[[1]]})
    }
    return(unique(unlist(sigs)))
}
