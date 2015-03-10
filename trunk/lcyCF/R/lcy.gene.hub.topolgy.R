lcy.gene.rank.topology <- function(g,data,edge.label,edge.weight){
    degree.sort         <- sort(igraph::degree(g,mode='out'),decreasing=T)
    degree.prot         <- names(degree.sort[degree.sort !=0])
    degree.prot.index   <- match(degree.prot, V(g)$name)

    score <- lapply(degree.prot,function(x){
        if(!x %in% rownames)
            return(NA)
        sub             <- lcy.graph.search(g,v.attr=list(name=x),gap=1,mode='down')
        score           <- lcy.graph.score(sub, data, edge.label=edge.label,edge.weight)
        return(score)
    })
    score <- do.call(rbind,score)
    rownames(score) <- degree.prot
    return(score)
}

