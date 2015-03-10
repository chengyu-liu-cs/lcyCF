lcy.graph.intersect <-
function(g.list,v.attr='', e.attr='',singleton=TRUE) {
#   * vertex should have 'name' attribute which is the labels of node. Use 'name' to distinguish different nodes.
#   * v.attr and e.attr should be separated by ','
#   * check whether all given graphs are directed or indirected graphs. They should be the same.
#   * check whether given vertex.attr exist in all the graphs

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
    Set         <- list() # save union graph of the whole graphs
    print('1')
    if(is.directed){
        # component of EDGE is a vector of strings
        Set <- EDGE[[1]]
        x <- lapply(EDGE[2:g.size], function(x){
                                    if(length(Set) ==0 ){
                                        return(NULL)
                                    }
                                    Set <<- intersect(Set,x)
                                    return(NULL)
                                    })
    }else{
        # component of EDGE is a matrix of strings
        Set <- EDGE[[1]]
        #system.time(
        x <- lapply(EDGE[2:g.size], function(x){
                                    if(nrow(Set) ==0 ){
                                        return(NULL)
                                    }
                                    x <- lapply(seq_len(nrow(x)), function(i) x[i,])
                                    z <- apply(Set,1, function(y){
                                            tf <- lapply(x, function(z){ 
                                                    tf  <- setequal(y,z);
                                                   })
                                             sum(unlist(tf))
                                         })
                                    index       <- which(z !=0)
                                    if(length(index) == 0){
                                        Set           <<- matrix(character(),nc=3)
                                        colnames(Set) <<- c('NodeA','NodeB','Edge')
                                    }else{
                                        Set   <<- Set[index,,drop=FALSE]
                                    }
                                    return(NULL)
                               })
    }
    g.out <- tmp.edgelist2graph(Set,is.directed,v.attr,e.attr)
    # considering intersection of individual nodes, those which are overlaped but their interactions are not overlapped.
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
    return(g.out)
}
