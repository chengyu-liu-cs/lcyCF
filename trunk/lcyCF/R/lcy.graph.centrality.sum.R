lcy.graph.centrality.sum <-
function(graph,mode='total',directed=TRUE, weights=NULL,scale=TRUE,normalized=FALSE){
# used to compute centrality of graph, including degree,betweenness, closeness and eigenvector.
#   
#   Note: degree centrality : compute "total" degree
#   betweenness/closeness centrality  : if weight is given, then centrality based on weights are computed.
#
#   degree(graph, v=V(graph), mode = c("all", "out", "in", "total"),loops = TRUE, normalized = FALSE)
#   betweenness(graph, v=V(graph), directed = TRUE, weights = NULL, nobigint = TRUE, normalized = FALSE)
#   closeness(graph, vids=V(graph), mode = c("out", "in", "all", "total"),weights = NULL, normalized = FALSE)
#   evcent (graph, directed = FALSE, scale = TRUE, weights = NULL,options = igraph.arpack.default)

    if(!is.null(weights)){
        if(weights %in% list.edge.attributes(graph))
            weights <- get.edge.attribute(graph,weights) + 1
        else
            weight <- NULL
    }
    graph <- set.vertex.attribute(graph, name='degree',value=igraph::degree(graph,mode=mode,normalized=normalized))
    graph <- set.vertex.attribute(graph, name='betweenness',value=betweenness(graph,directed=directed,weights=weights,normalized=normalized))
    graph <- set.vertex.attribute(graph, name='closeness',value=closeness(graph,mode=mode,weights=weights,normalized=normalized))
    #graph <- set.vertex.attribute(graph, name='eigenvector',value=evcent(graph,directed=FALSE,weights=weights,scale=scale))
    return(graph)
}
