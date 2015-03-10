lcy.get.vertex.attr <- function(g,attr){
    # get.vertex.attribute in igraph only support one attribute extraction.
    stopifnot(missing(g)|missing(attr)|is.igraph(g)|is.null(attr)|is.character(attr))
    attr        <- strsplit(attr,split=',')[[1]]
    attr.size   <- length(attr)
    if(attr.size == 0){
        stop('vertex attribute is empty.')
    }else{
        if(attr.size == 1){
            if(attr == '*'){
                attr <- list.vertex.attributes(g)
            }
        }
    }
    index <- match(attr, list.vertex.attributes(g))
    if(sum(!is.na(index))==0){
        print('The attribute NOT exist')
        return(NULL)
    }else{
        index <- index[which(!is.na(index))]
        if(length(index) != attr.size){
            attr    <- list.vertex.attributes(g)[index]
        }
    }
    y           <- lapply(attr,get.vertex.attribute, g=g)
    y           <- do.call(cbind,y)
    rownames(y) <- V(g)$name
    colnames(y) <- attr
    return(y)
}
