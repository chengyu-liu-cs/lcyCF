tmp.labeled.edgelist <- 
function(graph, edge.label='LinkType',vertex.activity=TRUE){

    if(vcount(graph)==0)
        return(NULL)
	edge.list   <- get.edgelist(graph,names=TRUE)
    index1      <- lcy.in(edge.list[,1],list=get.vertex.attribute(graph, 'name'))[,2]
    index2      <- lcy.in(edge.list[,2],list=get.vertex.attribute(graph, 'name'))[,2]

    if (vertex.activity == FALSE){
	    x       <- paste(edge.list[,1],edge.list[,2],get.edge.attribute(graph,edge.label), sep="->")
    }else{
        color   <- get.vertex.attribute(graph, 'color')
	    x       <- paste(paste(edge.list[,1],color[index1],sep='_'),paste(edge.list[,2],color[index2],sep='_'),get.edge.attribute(graph,edge.label),sep="->")
    }
    return(x)
}
