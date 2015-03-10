lcy.get.graph.attr <- function(g,attr){
    # get.edge.attribute in igraph only support one attribute extraction.
    stopifnot(missing(g)|missing(attr)|is.igraph(g)|is.null(attr)|is.character(attr))
    attr        <- strsplit(attr,split=',')[[1]]
    attr.size   <- length(attr)
    if(attr.size == 0){
        stop('edge attribute is empty.')
    }else if(attr.size == 1){
        if(attr == '*'){
            attr <- list.graph.attributes(g)
        }
    }
    index <- match(attr, list.graph.attributes(g))
    if(sum(!is.na(index))==0){
        print('The attribute NOT exist')
        return(NULL)
    }else{
        index <- index[which(!is.na(index))]
        if(length(index) != attr.size){
            attr    <- list.graph.attributes(g)[index]
        }
    }
    y           <- lapply(attr,get.graph.attribute, g=g)
    y           <- do.call(cbind,y)
    colnames(y) <- attr
    return(y)
}
