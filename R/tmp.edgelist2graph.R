tmp.edgelist2graph <-
function(Set, is.directed,v.attr,e.attr){
    # it is called in lcy.graph.intersect/union/complementary.
    # it calles tmp.matrix.strsplit2graph function to convert matrix 
    # convert edgelist to graph

    if(length(Set)==0){
        return(graph.empty())
    }
    if(is.directed){
        Set <- lapply(Set, function(x){strsplit(x,split='->')[[1]]})
        Set <- do.call(rbind,Set)
        # convert a matrix to graph. The matrix contains three columns at most(NodeA,NodeB,Edge). This matrixi the same with that in indirect.
        g.out <- tmp.matrix.strsplit2graph(Set,is.directed,v.attr,e.attr) 
    }else{
        # convert a matrix to graph. The matrix contains three columns at most(NodeA,NodeB,Edge). This matrixi the same with that in indirect.
        g.out <- tmp.matrix.strsplit2graph(Set,is.directed,v.attr,e.attr) 
    }
    return(g.out)
}
