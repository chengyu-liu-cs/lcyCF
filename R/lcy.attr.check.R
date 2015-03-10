lcy.attr.check <- function(g,v.attr, e.attr){
    # check attributes for vertex and edges

    v.attr.list     <- list.vertex.attributes(g)
    e.attr.list     <- list.edge.attributes(g)
    if(!missing(v.attr)){
        v.attr.size <- length(v.attr)
        v.attr      <- as.character(v.attr)
    }else{
        v.attr.size <- 0
    }
    if(!missing(e.attr)){
        e.attr.size <- length(e.attr)
        e.attr      <- as.character(e.attr)
    }else{
        e.attr.size <- 0
    }
    v.index         <- NULL
    e.index         <- NULL
    # check whether given vertex.attr exist in all the graphs
    if(v.attr.size != 0){
        v.index     <- match(v.attr, v.attr.list)
    }
    # check whether given edge.attr exist in all the graphs
    if(e.attr.size != 0){
        e.index     <- match(e.attr, e.attr.list)
    }
    if(is.null(v.index) & is.null(e.index)){
        print('vertex or edge attributes given are empty')
        return(NULL)
    }else if((!is.null(v.index)) & (!is.null(e.index))){
        return(list(v.attr=v.index,e.attr=e.index))
    }else if(is.null(v.index)){
        return(e.index)
    }else{
        return(v.index)
    }
}
