lcy.gSpan.subgraph.import <- function(file='gSpanInputData.csv.fp', num.node=3, n.process=4, group=list(nodes=c(),edges=c(),graphs=c()), vertex.activity=TRUE,activity.key='color',edge.label='linkType'){
    node.label.list <- group$nodes
    edge.label.list <- group$edges
    graph.label.list<- group$graphs
    num.edge.label  <- length(edge.label.list) 
    num.node.list   <- dim(node.label.list)[1]
    node.label.list[,2] <- as.character(node.label.list[,2])
    # read file
    str.graph       <- readLines(file)
    subgraph        <- list()
    if(length(str.graph)==0){
        print('There is not any subgraphs\n')
        return(subgraph)
    }
    # count the number of subgraphs, in order to parallelize
    sep.index <- c(1)
    i=1
    tmp <- lapply(str.graph,function(x){
                        if(x==""){
                            i <<- i + 1
                            sep.index <<- c(sep.index,i)
                        }else{
                            i <<- i + 1
                        }
                        return(NULL)
               })
    # allocate jobs
    nrd <- (length(sep.index)-length(sep.index)%%n.process)/n.process
    flag <- length(sep.index) %% n.process
    if(nrd == 0){
        n.process   <- 1
        nrd         <- flag
        flag        <- 0
    }
    if(flag!=0){
        n.slaves <- n.process + 1
        n.per.process <- c(rep(nrd,n.process),flag)
    }else{
        n.slaves <- n.process
        n.per.process <- rep(nrd,n.process)
    }
    mpi.spawn.Rslaves(nslaves=n.slaves)
    mpi.bcast.Robj2slave(n.per.process)
    mpi.bcast.Robj2slave(slave.apply.subgraph.read)
    mpi.bcast.Robj2slave(node.label.list)
    mpi.bcast.Robj2slave(edge.label.list)
    mpi.bcast.Robj2slave(graph.label.list)
    mpi.bcast.Robj2slave(num.edge.label)
    mpi.bcast.Robj2slave(num.node.list)
    mpi.bcast.Robj2slave(str.graph)
    mpi.bcast.Robj2slave(sep.index)
    mpi.bcast.Robj2slave(num.node)
    mpi.bcast.Robj2slave(vertex.activity)
    mpi.bcast.Robj2slave(activity.key)
    mpi.bcast.Robj2slave(edge.label)
    mpi.bcast.cmd(library(igraph))
    mpi.bcast.cmd(index <- mpi.comm.rank())
    tmp             <- mpi.remote.exec(slave.apply.subgraph.read())
    mpi.close.Rslaves()

    i <- 1
    out1 <-lapply(tmp, function(x){
            out2 <- lapply(x, function(y){
                subgraph[[i]] <<- y
                i <<- i + 1
                return(NULL)
            })
            return(NULL)
        })

    return(subgraph)
}

slave.apply.subgraph.read <- function(){
    if(index==1){
        start   <- 1
        end     <- n.per.process[index]-1
    }else{
        start   <- sum(n.per.process[1:(index-1)])
        end     <- sum(n.per.process[1:index])
    }
    graph           <- list()
    count           <- 1
    local.str.graph <- str.graph[sep.index[start]:(sep.index[end]-1)] 
    subgraph <- lapply(1:length(local.str.graph),function(i){
        print(local.str.graph[i])
        if(local.str.graph[i] !=''){
            print(i)
            str <- strsplit(local.str.graph[i],split=' ')
            print(str)
            if( str[[1]][1] =='t'){
                tmp.node.list <<- matrix(numeric(0),nc=2)
                tmp.edge.list <<- matrix(numeric(0),nc=3)
            }else if(str[[1]][1] == 'v'){
                tmp.node.list <<- rbind(tmp.node.list,c(as.numeric(str[[1]][2]),as.numeric(str[[1]][3])))
            }else if(str[[1]][1] == 'e'){
                tmp.edge.list <<- rbind(tmp.edge.list,c(as.numeric(str[[1]][2]),as.numeric(str[[1]][3]),as.numeric(str[[1]][4])))
            }else if(str[[1]][1] == 'x'){
                str <- str[[1]][-1]
                tmp.graph.list <<- as.numeric(str)
            }
        }else{
            subgraph                <- NULL
            if (nrow(tmp.node.list) >= num.node){
                subgraph            <- graph.empty()
                attr                <- list()
                if(vertex.activity ==TRUE){
                    #tmp <- matrix(unlist(strsplit(node.label.list[tmp.node.list[,2]+1,2],split='_')),nc=2,byrow=T)
                    tmp <- do.call(rbind,strsplit(node.label.list[tmp.node.list[,2]+1,2],split='_'))
                    attr$name               <- tmp[,1]
                    attr[[activity.key]]    <- tmp[,2]
                }else{
                    #tmp <- matrix(unlist(strsplit(node.label.list[tmp.node.list[,2]+1,2],split='_')),nc=1,byrow=T)
                    tmp <- do.call(rbind,strsplit(node.label.list[tmp.node.list[,2]+1,2],split='_'))
                    attr$name       <- tmp[,1]
                }
                # set graph attributes
                if(!is.null(graph.label.list)){
                    supp            <- paste(graph.label.list[tmp.graph.list+1],collapse=',')
                }else{
                    supp            <- paste(as.character(tmp.graph.list+1),collapse=',')
                }
                subgraph            <- set.graph.attribute(subgraph,name='supp',value=supp)
                freq                <- length(tmp.graph.list)/length(graph.label.list)
                subgraph            <- set.graph.attribute(subgraph,name='freq',value=freq)
                # adding vertices and attributes
                subgraph            <- add.vertices(subgraph,nv=nrow(tmp.node.list), attr=attr)
                # adding edges and also check the direction
                tf                  <- tmp.edge.list[,3] > num.edge.label
                index               <- which(tf)
                # larger than num.edge.label
                for(j in index){
                    if(tmp.edge.list[j,3] %% num.edge.label == 0){
                        tmp.edge.list[j,3]  <- num.edge.label 
                    }else{
                        tmp.edge.list[j,3]  <- tmp.edge.list[j,3] %% num.edge.label
                    }
                    if(tmp.node.list[tmp.edge.list[j,1]+1,2] < tmp.node.list[tmp.edge.list[j,2]+1,2]){
                        x                   <- tmp.edge.list[j,1]
                        tmp.edge.list[j,1]  <- tmp.edge.list[j,2]
                        tmp.edge.list[j,2]  <- x
                    }
                }
                # smaller than num.edge.label
                for(j in which(!tf)){
                    if(tmp.node.list[tmp.edge.list[j,1]+1,2] > tmp.node.list[tmp.edge.list[j,2]+1,2]){
                        x                   <- tmp.edge.list[j,1]
                        tmp.edge.list[j,1]  <- tmp.edge.list[j,2]
                        tmp.edge.list[j,2]  <- x
                    }
                }
                attr[[edge.label]] <- edge.label.list[tmp.edge.list[,3],2]
                graph[[count]] <<- add.edges(subgraph, edges=as.vector(t(tmp.edge.list[,c(1,2)]+1)),attr=attr)
                count <<- count + 1
            }
        }
    })
    return(graph)
}
