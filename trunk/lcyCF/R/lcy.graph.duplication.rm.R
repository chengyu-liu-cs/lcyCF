lcy.graph.duplication.rm.v2<- 
function(graph,vertex.activity=TRUE){
    # if two graphs have exactly the same nodes, they are considered as duplicated. this is used before lcy.subgraph.matrix.exprs called.
    
    if(length(graph) <= 1)
        return(graph)
    uni.graph <- list(graph[[1]])
    count <- 2
    tmp <- lapply(graph[2:length(graph)],function(x){
        if(vertex.activity == TRUE)
            x.id <- paste(V(x)$name,get.vertex.attribute(x,'color'),sep='_')
        else
            x.id <- paste(V(x)$name,sep='_')
        x.out <- lapply(uni.graph,function(y){
            if(vertex.activity == TRUE)
                y.id <- paste(V(y)$name,get.vertex.attribute(y,'color'),sep='_')
            else
                y.id <- paste(V(y)$name,sep='_')
            if(!setequal(x.id, y.id)){
                return(1)
            }else{
                return(0)
            }})
        if(sum(unlist(x.out)) == length(uni.graph)){
            uni.graph[[count]]  <<- x
            count               <<- count + 1
        }})
    return(uni.graph)
}


lcy.graph.duplication.rm <- 
function(graph,vertex.activity=TRUE,n.process=4){
    # if two graphs have exactly the same nodes, they are considered as duplicated. this is used before lcy.subgraph.matrix.exprs called.
    
    v.size <- lapply(graph, vcount)
    v.size <- unlist(v.size)
    index <- order(v.size)   
    graph <- graph[index]
    uniq.graph <- graph
    len.g <- length(graph)
    count <- 1
    repeat{
        uniq.graph <- call.slave(uniq.graph,n.process=n.process)
        count <- count + 1
        if(len.g > length(uniq.graph)){
            len.g <- length(uniq.graph)
            if(len.g <= 5000 | count == 5)
                break
        }else{
            break
        }
    }
    print('       parallelization is done.')
    uniq.graph <- lcy.graph.duplication.rm.v2(uniq.graph)   
    return(uniq.graph)
}
call.slave <- function(graph,vertex.activity=TRUE,n.process=4){
    graph.size <- length(graph)

    nrd <- (length(graph)-length(graph)%%n.process)/n.process
    flag <- length(graph) %% n.process
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
    mpi.bcast.Robj2slave(slave.apply.dup.rm)
    mpi.bcast.Robj2slave(graph)
    mpi.bcast.Robj2slave(vertex.activity)
    mpi.bcast.cmd(library(igraph))
    mpi.bcast.cmd(index <- mpi.comm.rank())
    tmp             <- mpi.remote.exec(slave.apply.dup.rm(graph,vertex.activity,n.process))
    #tmp             <- mpi.remote.exec(slave.apply.dup.rm())
    mpi.close.Rslaves()
    tmp.graph <- list()
    i <- 1
    out1 <-lapply(tmp, function(x){
            out2 <- lapply(x, function(y){
                tmp.graph[[i]] <<- y
                i <<- i + 1
            })
        })
    return(tmp.graph)
}
slave.apply.dup.rm <- function(graph,vertex.activity,n.process){
#slave.apply.dup.rm <- function(){
    if(index==1){
        start   <- 1
        end     <- n.per.process[index]
    }
    else{
        start   <- sum(n.per.process[1:(index-1)])+1
        end     <- sum(n.per.process[1:index])
    }
    g <- graph[start:end]
    uniq.graph <- list() 
    i <- 1
    while(length(g) !=0){
        uniq.graph[[i]] <- g[[1]]
        i <- i + 1
        if(vertex.activity == TRUE)
            x.id <- paste(V(g[[1]])$name,get.vertex.attribute(g[[1]],'color'),sep='_')
        else
            x.id <- paste(V(g[[1]])$name,sep='_')
        out <- lapply(g,function(y){
                if(vertex.activity == TRUE)
                    y.id <- paste(V(y)$name,get.vertex.attribute(y,'color'),sep='_')
                else
                    y.id <- paste(V(y)$name,sep='_')
                if(setequal(x.id, y.id)){
                    return(1)
                }else{
                    return(0)
                }
            })
        y.index <- which(unlist(out)==1)
        g <- g[-y.index]
    }
    return(uniq.graph)
}
