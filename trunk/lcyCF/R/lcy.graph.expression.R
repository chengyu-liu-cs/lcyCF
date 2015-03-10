lcy.graph.expression <- function(graph, activity, up=2, down=-2, weight=NULL, edge.label=NULL, neutral=FALSE,g.name='graph'){
    if(missing(graph)|missing(activity))
        stop('Neither graph nor data can be missing')
    if(ncol(activity) < 2){
        stop('activity should have two columns. First column is label, and second column is its activity. -1 and 1 are for down and up, respectively.')
    }
    # linkType edge attribute must exist, which are used to check express direction.
    if(!is.null(weight)){
        names <- weight[,1]
        weight <- weight[,2]
        names(weight) <- names
    }
    data                <- as.matrix(as.numeric(activity[,2,drop=FALSE]))
    rownames(data)      <- activity[,1]
    indexup             <- data[,1] >= up
    indexdown           <- data[,1] <= down
    indexnot            <- !(indexup | indexdown)
    data[indexup,1]     <- 1
    data[indexdown,1]   <- -1
    data[indexnot,1]    <- 0
    if(!neutral){ # neutral gene is not included.
        data    <- data[indexup | indexdown, ,drop=FALSE]
    }
    genes       <- intersect(V(graph)$name,rownames(data))
    # remove genes which do not have expression data
    graph       <- induced.subgraph(graph,vids=genes,"copy_and_delete")
    # remove genes which are singletons. 
    graph       <- delete.vertices(graph,which(degree(graph) <1))
    x           <- cbind(get.edgelist(graph),lcy.get.edge.attr(graph,"*"))
    colnames(x) <- c('x','y',list.edge.attributes(graph))
    # check whether the activity of genes match with regulations( activate or inhibit)
    if(!is.null(weight) & (!neutral)){
        if(edge.label %in% list.edge.attributes(graph)){
            index   <- sign(data[x[,1],1] * data[x[,2],1]) == weight[x[,edge.label]]
            x       <- x[index,,drop=FALSE]
        }else{
            stop('edge.label does no exist.')
        }
    }
    y           <- lcy.get.vertex.attr(graph,"*")
    graph       <- graph.data.frame(d=as.data.frame(x),directed=T)
    graph       <- lcy.set.vertex.attr(graph,attr=y)
    genes       <- V(graph)$name
    # add attribute color attribute
    color       <- c('red','green','gray65')
    names(color)<- c(1,-1,0)
    tmp         <- color[as.character(data[genes,1])]
    graph       <- set.vertex.attribute(graph,name='color',index=V(graph),value=tmp)
    graph       <- set.graph.attribute(graph, name='name',value=g.name)
    return(graph)
}

