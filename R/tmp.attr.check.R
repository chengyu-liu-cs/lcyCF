tmp.attr.check <-
function(g.list,v.attr,e.attr){
    # check attributes for vertex and edges

    g.size      <- length(g.list)
    v.attr.size <- length(v.attr)
    e.attr.size <- length(e.attr)
    # check whether given vertex.attr exist in all the graphs
    if(v.attr.size != 0){
        y <- lapply(g.list, function(x){
                            y <- list.vertex.attributes(graph=x)
                            if(length(y)==0){
                                stop('The length of vertex attributes is zero. however, the given vertex attribute is NON-empty')
                            }
                            sum(v.attr %in% y) == v.attr.size
                            })
        if(sum(unlist(y)) != g.size){
            stop('Not all the graphs have the v.attr')
        }
    }
    # check whether given edge.attr exist in all the graphs
    if(e.attr.size != 0){
        y <- lapply(g.list, function(x){
                            y <- list.edge.attributes(graph=x)
                            if(length(y)==0){
                                stop('The length of edge attributes is zero. however, the given edge attribute is NON-empty')
                            }
                            sum(e.attr %in% y) == e.attr.size
                            })
        if(sum(unlist(y)) != g.size){
            stop('Not all the graphs have the e.attr')
        }
    }
}
