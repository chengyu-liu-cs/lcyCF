lcy.graph.score <- function(g, data, edge.label, edge.weight){
    #  used to compute score for a given network, network, the network could be directed or undirected.
    #  edge.weight should be a two column matrix of data frame. the first column is edge name used in graph. the second column is the actual weight used for calculation.

    rownames <- rownames(data)
    if(missing(g) | missing(data)){
        stop('Neither graph nor data can be missing')
    }
    el <- get.edgelist(g)
    if(is.directed(g)){# directed then take topology into account
        if((!missing(edge.label)) & (edge.label %in% list.edge.attributes(g))){
            el <- cbind(el, get.edge.attribute(g,edge.label))
            if((!missing(edge.weight)) & (dim(edge.weight)[2] == 2)){
                rownames(edge.weight) <- edge.weight[,1]
                weighted <- TRUE
                index <- el[,3] %in% edge.weight[,1]
                el <- el[index,,drop=FALSE]
            }else{
                weighted <- FALSE
            }
        }else{
            warning('edge.label does not exist in the graph.\nDefault weight 1 is used.')
            weighted <- FALSE
        }
        index   <- el[,1] %in% rownames
        el      <- el[index,,drop=FALSE]
        index   <- el[,2] %in% rownames
        el      <- el[index,,drop=FALSE]
        if(nrow(el) < 1){
            return(NULL)
        }
        source  <- data[el[,1],,drop=FALSE]
        target  <- data[el[,2],,drop=FALSE]
        if(weighted){
            #res     <- colMeans(source + target * as.numeric(edge.weight[el[,3],2]),na.rm=TRUE)
            #res     <- colMeans(abs(target),na.rm=TRUE)
            res     <- sqrt(colSums(target^2,na.rm=TRUE))
        }else{
            #res     <- colMeans(source + target,na.rm=TRUE)
            res     <- sqrt(colSums(target^2,na.rm=TRUE))
        }
    }else{ # just take mean/median of interactomes
        genes <- V(g)$name
        genes   <- intersect(genes, rownames)
        res     <- apply(data[genes,,drop=FALSE], 2,mean)
    }
    return(res)
}


