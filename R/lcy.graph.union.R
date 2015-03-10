lcy.graph.union <-
function(g.list,v.attr='',e.attr='',singleton=TRUE,weighting=TRUE) {
#   * vertex should have 'name' attribute which is the labels of node. Use 'name' to distinguish different nodes.
#   * e.attr should be separated by ','. v.attr was not support. different attributes of v.attr results in different vertices. vertex attributes can not be considered. for example gene MYC in one network is up, while down in other netowrk. 
#   * check whether all given graphs are directed or indirected graphs. They should be the same.
#   * check whether given e.attr exist in all the graphs

    g.size      <- length(g.list)
    if(g.size <= 1){
        stop('at least two graph should be provided')
    }
    v.attr      <- strsplit(v.attr,split=',')[[1]]
    v.attr.size <- length(v.attr)
    e.attr      <- strsplit(e.attr,split=',')[[1]]
    # check empty igraph objct and remove from the list
    index <- unlist(lapply(g.list, vcount)) != 0
    g.list <- g.list[index]
    e.attr.size <- length(e.attr)
    # check whether all given graphs are directed or indirected graphs. They should be the same.
    is.directed    <- tmp.is.directed.graphlist(g.list)
    # check whether given vertex.attr exist in all the graphs
    tmp.attr.check(g.list,v.attr,e.attr)
    # construct a matrix which contains vertices, edges and their corresponding attributes.
    EDGE        <- tmp.prepare.set(g.list,v.attr,e.attr,is.directed)
    Set         <- list() # save union graph of the whole graphs
    if(is.directed){
        # component of EDGE is a vector of strings
        Set <- EDGE[[1]]
        x <- lapply(EDGE[2:g.size], function(x){
                                    if(length(Set) ==0 ){
                                        return(NULL)
                                    }
                                    Set <<- union(Set,x)
                                    return(NULL)
                                    })
    }else{
        # component of EDGE is a matrix of strings
        Set <- EDGE[[1]]
        #system.time(
        x <- lapply(EDGE[2:g.size], function(x){
                                    z <- apply(x,1, function(y){tf <- apply(Set,1, function(z){ tf  <- setequal(y,z)});sum(tf)})
                                    index       <- which(z ==0)
                                    if(length(index) != 0){
                                        Set           <<- rbind(Set, x[index,])
                                        return(NULL)
                                    }else{
                                        return(NULL)
                                    }
                               })
    }
    g.out <- tmp.edgelist2graph(Set,is.directed,v.attr,e.attr)
    # in case, there are singletons which do not have any interactions
    if(singleton){
        if(is.directed){
            type <- 'upper'
        }else{
            type <- 'both'
        }
        g.adjacency     <- lapply(g.list,get.adjacency,sparse=FALSE,type=type)
        attr            <- list()
        attr[['name']]  <- c()
        y <- lapply(g.adjacency,function(x){
                                rownames  <-rownames(x)
                                index     <- which(rowSums(x)==0 & colSums(x) == 0)
                                if(length(index)!=0){
                                    names <- rownames[index]
                                    index <- which(!names %in% attr[['name']])
                                    attr[['name']] <<- c(attr[['name']], names[index])
                                }
                                })
        # adding intersect vertices which do not have intersect edges but which are intersect vertices.
        if(!is.null(attr[['name']])){
            g.out   <- add.vertices(g.out, nv=length(attr[['name']]), attr=attr)
        }
    }
    if(weighting){
        g.out   <- set.edge.attribute(g.out,name='weight',value=0)
        el      <- tmp.prepare.set(list(g.out),v.attr,e.attr,is.directed)
        tmp     <- lapply(EDGE,function(x) {
                        if(is.directed){
                            index <- el[[1]] %in% x
                        }else{
                            index <- lcy.is.matrix.in(el[[1]], x,order=is.directed)
                        }
                        E(g.out)$weight[index] <<- E(g.out)$weight[index] + 1
                        return(NULL)
                    })
    }
    return(g.out)
}
