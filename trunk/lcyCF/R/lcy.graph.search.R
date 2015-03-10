lcy.graph.search <- function(g,v.attr=list(), e.attr=list(), mode=c('all','down','up'),gap=1, e.color, rm.singleton=TRUE){#,v.func=NULL,e.func=NULL){
    if(missing(mode))
        mode <- 'all'
    if(is.list(e.attr) & length(e.attr) > 0){
        # check existance of attributes.
        e.attr.names    <- names(e.attr)
        e.attr.size     <- length(e.attr.names)
        index           <- match(e.attr.names, list.edge.attributes(g))
        if(sum(!is.na(index)) != 0){
            if(sum(!is.na(index)) != e.attr.size){
                i               <- is.na(index)
                non.attr        <- e.attr.names[i]
                e.attr.names    <- e.attr.names[!i]
                msg             <- 'Edge attributes'
                tmp             <- lapply(non.attr, function(i){ x <- sprintf(', %s', i);msg <<- paste(msg,x,sep='');return(NULL)})
                msg             <- paste(msg,' DONOT exist in the graph.')
                warning(msg)
        }
        y <- lapply(e.attr.names, function(x){
                    value       <- e.attr[[x]]
                    i           <- match(get.edge.attribute(g,x),value)
                    return(which(!is.na(i)))
                    })
            e.index   <- y[[1]]
            z <- lapply(y, function(x){ e.index <<- intersect(x,e.index)})
            g <- subgraph.edges(g,eids=e.index)
            if(missing(v.attr) | length(v.attr) < 1){
                v <- V(g)$name 
            }
        }else{
            warning('NONE of Edge attributes exists')
        }
    }
    ## vertex attributes process
    if(is.list(v.attr) & length(v.attr) > 0){
        v.attr.names <- names(v.attr)
        v.attr.size <- length(v.attr.names)
        index   <- match(v.attr.names, list.vertex.attributes(g))
        if(sum(!is.na(index)) != 0){
            if(sum(!is.na(index)) != v.attr.size){
                i <- is.na(index)
                non.attr <- v.attr.names[i]
                v.attr.names <- v.attr.names[!i]
                msg <- 'Vertex attributes'
                tmp <- lapply(non.attr, function(i){ x <- sprintf(', %s', i);msg <<- paste(msg,x,sep='');return(NULL)})
                msg <- paste(msg,' DONOT exist in the graph.')
                warning(msg)
            }
            y <- lapply(v.attr.names, function(x){
                    value <- v.attr[[x]]
                    i <- match(get.vertex.attribute(g,x),value)
                    return(which(!is.na(i)))
                    })
            v.index   <- y[[1]]
            z <- lapply(y, function(x){ v.index <<- intersect(x,v.index)})
            v <- V(g)$name[v.index]
        }else{
            warning('NONE of vertex attributes exists')
        }
        ## consider edge, gap=0, induced sugraphs. mode is valid only if gap!=0
        init.v <- v
    }
    if(gap > 0){
        if(mode=='down'){
            y   <- lapply(1:gap, function(x){
                        eids    <- E(g) [ from(v) ]
                        el      <- get.edgelist(g)[eids,,drop=FALSE]
                        vl      <- el[,2]
                        v       <<- setdiff(vl, v)
                        return(eids)
                        })
            g.out <- subgraph.edges(g,eids=unlist(y))
        }else if(mode=='up'){
            y   <- lapply(1:gap, function(x){
                        eids    <- E(g) [ to(v) ]
                        el      <- get.edgelist(g)[eids,,drop=FALSE]
                        vl      <- el[,1]
                        v       <<- setdiff(vl, v)
                        return(eids)
                        })
            g.out <- subgraph.edges(g,eids=unlist(y))
        }else{
            y <- lapply(1:gap, function(x){
                        eids    <- E(g) [ from(v)| to(v) ]
                        el      <- get.edgelist(g)[eids,,drop=FALSE]
                        vl      <- unique(as.vector(el))
                        v       <<- setdiff(vl, v)
                        return(eids)
                        })
            g.out <- subgraph.edges(g,eids=unlist(y))
        }
    }else{
        g.out <- induced.subgraph(g, vids=v, impl="copy_and_delete")
    }
    if(rm.singleton){
        g.out <- delete.vertices(g.out,which(degree(g.out) <1))
    }
    if(sum(is.na(match('linkType', list.edge.attributes(g.out))))){
        if(missing(e.color)){
            e.color       <- c('red','green','blue')
            names(e.color) <- c('gene expression','gene repression','protein-protein interaction')
        }
        g.out       <- set.edge.attribute(graph=g.out, name='color',value=e.color[E(g.out)$linkType])
    }
    return(g.out)
}
