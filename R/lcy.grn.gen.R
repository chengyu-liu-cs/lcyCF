lcy.grn.gen <- function(genes,activity=NULL,gap=0,mode = c("all", "down", "up"),rm.singleton=TRUE,up.bound=1,low.bound=-1){
    data(lcyCF)
    if(missing(mode))
        mode    <- 'all'
    if(!is.null(activity)){
        genes   <- activity[,1]
        g       <- lcy.graph.search(moksiskaan,v.attr=list(name=genes),e.attr=list(linkType=c('gene expression','gene repression')),gap=gap,mode=mode,rm.singleton=rm.singleton)
        if(ncol(activity)>=2){
            g   <- lcy.graph.expression(g,activity,up=up.bound,down=low.bound)
        }
    }else if(!missing(genes)){
        genes   <- as.vector(genes)
        g       <- lcy.graph.search(moksiskaan,v.attr=list(name=genes),gap=gap,mode=mode,rm.singleton=rm.singleton)
    }else{
        stop('input is missing, either genes or activity should be provided')
    }
    return(g)
}

