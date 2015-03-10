lcy.gSpan.input.gen <- function(graph.list, file='gSpanInputData.csv', edge.label='linkType',vertex.activity=TRUE,activity.key='color'){
    node.label.list <- c()
    edge.label.list <- c()
    if(file.exists(file))
        file.remove(file)
    if(vertex.activity == FALSE){
        for(i in 1:length(graph.list)){
            node.label.list <- union(node.label.list, V(graph.list[[i]])$name)
            edge.label.list <- union(edge.label.list, get.edge.attribute(graph.list[[i]], edge.label))
        }
    }else{
        for(i in 1:length(graph.list)){
            node.label.list <- union(node.label.list, paste(V(graph.list[[i]])$name, get.vertex.attribute(graph.list[[i]],activity.key),sep='_'))
            edge.label.list <- union(edge.label.list, get.edge.attribute(graph.list[[i]], edge.label))
        }
    }
    node.index  <- 0:(length(node.label.list)-1)
    return.list <- list()
    edge.index  <- 1:(length(edge.label.list))
    return.list$edges <- data.frame(index=edge.index, label=edge.label.list,stringsAsFactors=FALSE)
    return.list$nodes <- data.frame(index=node.index, label=node.label.list,stringsAsFactors=FALSE)

    graph.name  <- c()
    for (i in 1:length(graph.list)){
        graph.name      <- c(graph.name,get.graph.attribute(graph.list[[i]],'name'))
        node.list       <- c('t', '#', i-1)
        if(vertex.activity)
            tmp.node.label  <- paste(V(graph.list[[i]])$name, get.vertex.attribute(graph.list[[i]],activity.key),sep='_')
        else
            tmp.node.label  <- V(graph.list[[i]])$name
        tmp             <- match(tmp.node.label,node.label.list)
        node.list       <- rbind(node.list,cbind('v', 0:(length(tmp)-1),tmp-1))
        rownames(node.list) <- NULL
        # separate by space
        write.table(node.list,file=file,sep=' ',row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
        node.list       <- cbind(0:(length(tmp)-1),tmp-1)
        # appending edge at the end of vertex.
        tmp.edge.list <- get.edgelist(graph.list[[i]])
        edge.list <- data.frame(e=rep('e',nrow(tmp.edge.list)),
                                P=match(tmp.edge.list[,1], V(graph.list[[i]])$name)-1,
                                Q=match(tmp.edge.list[,2], V(graph.list[[i]])$name)-1,
                                L=match(get.edge.attribute(graph.list[[i]],edge.label), edge.label.list))
        rownames(edge.list) <- NULL
        # since gSpan does not support directed graph, so we need to do something so that it supports it.
        # if source node id larger than target id
        for(j in 1:nrow(edge.list)){
            if (node.list[edge.list[j,2]+1,2] > node.list[edge.list[j,3]+1,2])
                edge.list[j,4] = edge.list[j,4] + length(edge.label.list)
        }
        # separate by space
        write.table(edge.list,file=file,sep=' ',row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
    }
    return.list$graphs <- graph.name
    print(return.list$graphs)
    return(return.list)
}
