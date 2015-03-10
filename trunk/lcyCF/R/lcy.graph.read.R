lcy.graph.read <- 
function(path='.', file.list=NULL, format='graphml',vertex.name, vertex.label,vertex.attr.rm, edge.label, edge.attr.rm, graph.attr.rm){
    cur.path <- getwd()
    setwd(path)

    if(is.null(file.list)){
        switch(format,
            graphml={ 
                file.list <- list.files(pattern='*.xml')
                file.list <- rbind(file.list, list.files(pattern='*.graphml'))
            },
            gml={
                file.list <- list.files(pattern='*.gml')
            },
            edgelist={
                file.list <- list.files(pattern='*.csv')
            },
            {
                stop('format is not supported\n')
            }
        )
    }
    graph               <- list()
    # read graph filesi, graphml format.	
    for (i in file.list){
    	graph[[i]]  <- read.graph(i, format='graphml');
        graph[[i]]  <- set.graph.attribute(graph[[i]], "name", i)
        if(vcount(graph[[i]]) == 0)
            next
    	if(!missing(vertex.name)){
            if(vertex.name %in% list.vertex.attributes(graph[[i]])){
    	    	node.name           <- get.vertex.attribute(graph[[i]], vertex.name)
    	    	V(graph[[i]])$name  <- node.name
    	    }else {
    	        warning(paste('Invalid nodename parameter: ', vertex.name,".", sep=""))
    	    }
        }
    	if(!missing(vertex.label)){
            if(vertex.label %in% list.vertex.attributes(graph[[i]])){
    	    	node.name           <- get.vertex.attribute(graph[[i]], vertex.label)
    	    	V(graph[[i]])$label <- node.name
    	    }else {
    	        warning(paste('Invalid nodename parameter: ', vertex.label,".", sep=""))
    	    }
        }
        if(!missing(vertex.attr.rm)){
            index <- vertex.attr.rm %in% list.vertex.attributes(graph[[i]])
            if(sum(index)!=length(vertex.attr.rm)){
                warning(paste("vertex attribute :", paste(vertex.attr.rm[!index], sep=','),' not exist', sep=''))
            }
            vertex.attr.rm <- vertex.attr.rm[index]
            for(j in 1:length(vertex.attr.rm)){
                graph[[i]] <- remove.vertex.attribute(graph[[i]],name=vertex.attr.rm[j])
            }
        }
    	# Check whether the given edge.label is an attribute of vertex of the graph
        # has potential error rist in following three lines. I should reset the attribute of edge.label to 'LinkType'. 
        # Since in lcy.common.edge* function I assume the attribute name is 'LinkType'.
        if(!missing(edge.label)){
    	    if (edge.label != "" & !edge.label %in% list.edge.attributes(graph[[i]])){
    	        warning(paste('Invalid nodename parameter: ', edge.label,".", sep=""))
    	    }else{
                V(graph[[i]])$label <- get.edge.attribute(graph[[i]],name=edge.label)
            }
        }
        if(!missing(edge.attr.rm)){
            index <- edge.attr.rm %in% list.edge.attributes(graph[[i]])
            if(sum(index)!=length(edge.attr.rm)){
                warning(paste("edge attribute :", paste(edge.attr.rm[!index], sep=','),' not exist', sep=''))
            }
            edge.attr.rm <- edge.attr.rm[index]
            for(j in 1:length(edge.attr.rm)){
                graph[[i]] <- remove.edge.attribute(graph[[i]],name=edge.attr.rm[j])
            }
        }
        if(!missing(graph.attr.rm)){
            index <- edge.attr.rm %in% list.graph.attributes(graph[[i]])
            if(sum(index)!=length(graph.attr.rm)){
                warning(paste("graph attribute :", paste(graph.attr.rm[!index], sep=','),' not exist', sep=''))
            }
            graph.attr.rm <- graph.attr.rm[index]
            for(j in 1:length(graph.attr.rm)){
                graph[[i]] <- remove.graph.attribute(graph[[i]],name=graph.attr.rm[j])
            }
        }
    }
    setwd(cur.path)
    return(graph)
}
