tmp.matrix.strsplit2graph <-
function(Set,is.directed,v.attr, e.attr){
    if(length(v.attr) != 0){
        z1 <- lapply(Set[,1], function(x){strsplit(x,split='::')[[1]]})
        z2 <- lapply(Set[,2], function(x){strsplit(x,split='::')[[1]]})
        z1 <- do.call(rbind,z1)
        z2 <- do.call(rbind,z2)
    }else{
        z1 <- Set[,1,drop=FALSE]
        z2 <- Set[,2,drop=FALSE]
    }
    if(length(e.attr)!= 0){
        z3 <- lapply(Set[,3], function(x){strsplit(x,split='::')[[1]]})
        z3 <- do.call(rbind,z3)
    }else{
        z3 <- NULL
    }
    
    e.list              <- cbind(z1[,1],z2[,1],z3)
    colnames(e.list)    <- c('A','B',e.attr)
    v.list              <- rbind(z1,z2)
    v.list              <- unique(v.list)
    colnames(v.list)    <- c('name',v.attr)
    g.set               <- graph.data.frame(data.frame(e.list), directed=is.directed,vertices=as.data.frame(v.list))
    return(g.set)
}
