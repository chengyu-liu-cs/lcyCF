lcy.is.edge.in <- function(g,graph,v.attr='',e.attr=''){

    v.attr      <- strsplit(v.attr,split=',')[[1]]
    v.attr.size <- length(v.attr)
    e.attr      <- strsplit(e.attr,split=',')[[1]]
    e.attr.size <- length(e.attr)

    # check whether all given graphs are directed or indirected graphs. They should be the same.
    is.directed    <- tmp.is.directed.graphlist(list(g,graph))
    # check whether given vertex.attr exist in all the graphs
    tmp.attr.check(list(g,graph),v.attr,e.attr)
    # collapse edges into string including vertix attributes
    EDGE        <- tmp.prepare.set(list(g,graph),v.attr,e.attr,is.directed)
    index <- lcy.is.matrix.in(EDGE[[2]], EDGE[[1]])
    return(which(index))
}


