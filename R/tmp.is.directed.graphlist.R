tmp.is.directed.graphlist <-
function(g.list){
    # g.list : igraph object list

    g.size <- length(g.list)
    y   <- lapply(g.list,is.directed)
    if(sum(unlist(y)) == g.size){
        directed <- TRUE
    }else if (sum(unlist(y)) == 0){
        directed <- FALSE
    }else{
        stop('Not all the graphs are directed or indirected graphs.')
    }
    return(directed)
}
