lcy.graph.complemenary <-
function(g1, g2,v.attr='', e.attr='',singleton=TRUE) {
#   * vertex should have 'name' attribute which is the labels of node. Use 'name' to distinguish different nodes.
#   * v.attr and e.attr should be separated by ','
#   * check whether all given graphs are directed or indirected graphs. They should be the same.
#   * check whether given vertex.attr exist in all the graphs

    g.list      <- list(g1,g2)
    g.size      <- length(g.list)
    if(g.size <= 1){
        stop('at least two graph should be provided')
    }
    v.attr      <- strsplit(v.attr,split=',')[[1]]
    v.attr.size <- length(v.attr)
    e.attr      <- strsplit(e.attr,split=',')[[1]]
    e.attr.size <- length(e.attr)
    # check whether all given graphs are directed or indirected graphs. They should be the same.
    is.directed    <- tmp.is.directed.graphlist(g.list)
    # check whether given vertex.attr exist in all the graphs
    tmp.attr.check(g.list,v.attr,e.attr)
    # construct a matrix which contains vertices, edges and their corresponding attributes.
    EDGE        <- tmp.prepare.set(g.list,v.attr,e.attr,is.directed)
    #print(EDGE[[1]][1:3])
    Set         <- list() # save union graph of the whole graphs
    if(is.directed){
        # component of EDGE is a vector of strings
        Set[[1]] <- setdiff(EDGE[[1]],EDGE[[2]])
        Set[[2]] <- setdiff(EDGE[[2]],EDGE[[1]])

    }else{
        # component of EDGE is a matrix of strings
        index1   <- lcy.is.matrix.in(EDGE[[1]],EDGE[[2]],byrow=TRUE,order=FALSE)
        index2   <- lcy.is.matrix.in(EDGE[[2]],EDGE[[1]],byrow=TRUE,order=FALSE)
        Set[[1]] <- EDGE[[1]][!index1,,drop=FALSE]
        Set[[2]] <- EDGE[[2]][!index2,,drop=FALSE]
        #print(sum(index2))
        #print(sum(index1))
    }
    g.out1 <- tmp.edgelist2graph(Set[[1]],is.directed,v.attr,e.attr)
    g.out2 <- tmp.edgelist2graph(Set[[2]],is.directed,v.attr,e.attr)
    # considering intersection of individual nodes, those which are overlaped but their interactions are not overlapped.
    g.out <- g.out1
    if(singleton){
        # adding intersect vertices which do not have intersect edges but which are intersect vertices.
        v.list      <- lapply(g.list, function(x){
                                        v <- V(x)$name
                                        z <- lapply(as.list(v.attr), get.vertex.attribute, graph=x)
                                        z <- do.call(cbind,z)
                                        z <- cbind(v,z)
                                        z <- apply(z,1,paste,collapse='::')
                                        })
        v.intersect <- v.list[[1]]
        y   <- lapply(v.list[2:g.size], function(x){
                                        v.intersect <<-intersect(x,v.intersect)
                                        return(NULL)
                                        })
        y   <- lapply(as.list(v.intersect),function(x){ y <- strsplit(x,split='::')[[1]]})
        y   <- do.call(cbind,y)
        if(ncol(y) !=0){
            index   <- which(y[1,] %in% V(g.out)$name==FALSE)
            rownames(y) <- c('name',v.attr)
            attr    <- split(y[,index], rownames(y))
            g.out   <- add.vertices(g.out, nv=length(index), attr=attr)
        }
    }
    g.out1  <- g.out
    g.out   <- g.out2
    if(singleton){
        # adding intersect vertices which do not have intersect edges but which are intersect vertices.
        v.list      <- lapply(g.list, function(x){
                                        v <- V(x)$name
                                        z <- lapply(as.list(v.attr), get.vertex.attribute, graph=x)
                                        z <- do.call(cbind,z)
                                        z <- cbind(v,z)
                                        z <- apply(z,1,paste,collapse='::')
                                        })
        v.intersect <- v.list[[1]]
        y   <- lapply(v.list[2:g.size], function(x){
                                        v.intersect <<-intersect(x,v.intersect)
                                        return(NULL)
                                        })
        y   <- lapply(as.list(v.intersect),function(x){ y <- strsplit(x,split='::')[[1]]})
        y   <- do.call(cbind,y)
        if(ncol(y) !=0){
            index   <- which(y[1,] %in% V(g.out)$name==FALSE)
            rownames(y) <- c('name',v.attr)
            attr    <- split(y[,index], rownames(y))
            g.out   <- add.vertices(g.out, nv=length(index), attr=attr)
        }
    }
    g.out2  <- g.out
    return(list(g1=g.out1,g2=g.out2))
}
