g.test.gen <-
function(){
    g   <- list()
    #--------------------
    # g1
    g[[1]]  <- graph(c(1,2,2,3,3,4),directed=FALSE)
    g[[1]]  <- set.vertex.attribute(g[[1]],name='name',value=LETTERS[1:4])
    g[[1]]  <- set.vertex.attribute(g[[1]],name='label',value=LETTERS[1:4])
    g[[1]]  <- set.vertex.attribute(g[[1]],name='color',value=rep('red',4))
    g[[1]]  <- set.edge.attribute(g[[1]],name='label',value=rep('GE',3))
    g[[1]]  <- set.edge.attribute(g[[1]],name='color',value=rep('gold',3))

    #--------------------
    # g2
    g[[2]]  <- graph.star(5,mode='undirected')
    g[[2]]  <- set.vertex.attribute(g[[2]],name='name',value=LETTERS[1:5])
    g[[2]]  <- set.vertex.attribute(g[[2]],name='label',value=LETTERS[1:5])
    g[[2]]  <- set.vertex.attribute(g[[2]],name='color',value=rep('red',5))
    g[[2]]  <- set.edge.attribute(g[[2]],name='label',value=rep('GE',4))
    g[[2]]  <- set.edge.attribute(g[[2]],name='color',value=rep('gold',4))

    #--------------------
    # g3
    g[[3]]  <- graph(c(1,2,2,1,2,3,3,4,4,5,5,6,6,1,1,4),directed=FALSE)
    g[[3]]  <- set.vertex.attribute(g[[3]],name='name',value=LETTERS[1:6])
    g[[3]]  <- set.vertex.attribute(g[[3]],name='label',value=LETTERS[1:6])
    g[[3]]  <- set.vertex.attribute(g[[3]],name='color',value=c(rep('red',3),'green','red','green'))
    g[[3]]  <- set.edge.attribute(g[[3]],name='label',value=c(rep('GE',3),'GR','GR','GR','GR','GR'))
    g[[3]]  <- set.edge.attribute(g[[3]],name='color',value=c(rep('gold',3),'blue','blue','blue','blue','blue'))

    return(g)
}
