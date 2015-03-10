tmp.prepare.set <- function(g.list,v.attr,e.attr,is.directed){
    # construct a matrix which contains vertices, edges and their corresponding attributes.
    # if the graphs are directed, then it returns a string vector. if indirected, then it returns a matrix with three columns (NodeA,NodeB,Edges)
    # if graphs are indirected, then remove duplicated edges(technical issue. kepp remain).
    # return a list for each graphs
    y <- lapply(g.list, function(x){
                        if('name' %in% list.vertex.attributes(x)){
                            v.list      <- get.vertex.attribute(x, name='name')
                        }else{
                            V(x)$name   <- V(x)
                            v.list      <- V(x)
                        }
                        e.list  <- get.edgelist(x,names=TRUE)
                        index1  <- sapply(e.list[,1],function(x) { match(x,v.list)}) 
                        index2  <- sapply(e.list[,2],function(x) { match(x,v.list)})
                        # select vertices with attributes for vertex1
                        z       <- lapply(as.list(v.attr), get.vertex.attribute, graph=x)
                        z       <- do.call(cbind,z)
                        z1      <- z[index1,,drop=FALSE]
                        z1      <- cbind(e.list[,1],z1)
                        z1      <- apply(z1,1,paste,collapse='::')
                        # select vertices with attributes for vertex2
                        z2      <- z[index2,,drop=FALSE]
                        z2      <- cbind(e.list[,2],z2)
                        z2      <- apply(z2,1,paste,collapse='::')
                        # select edges with attributes
                        e       <- lapply(as.list(e.attr),get.edge.attribute,graph=x)
                        e       <- do.call(cbind,e)
                        if(!is.null(e)){
                            e       <- apply(e,1,paste,collapse='::')
                            colnames<- c('NodeA','NodeB','Edge')
                        }else{
                            colnames<- c('NodeA','NodeB')
                        }
                        #--------------------------------------------------------#
                        # combine node A, node B and edge in order
                        z           <- cbind(z1,z2,e)
                        colnames(z) <- colnames
                        if(is.directed){
                            z <- apply(z, 1, paste,collapse='->')
                        }
                        return(z)
                        })
    if(!is.null(names(g.list))){
        names(y) <- names(g.list)
    }else{
        names(y) <- as.character(1:length(g.list))
    }
    return(y)                 
}
