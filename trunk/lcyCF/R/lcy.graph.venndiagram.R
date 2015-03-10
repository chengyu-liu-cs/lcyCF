lcy.graph.venndiagram <-
function(g.list,v.attr='',e.attr='',type='ellipses',doWeights=F, doEuler=F,colourAlgorithm='sequential',venn.out='vennDiagram.pdf'){
	#require(Vennerable)
	SetList.new <- function(set.names=NULL, set.contents=NULL) {
	    stopifnot(is.list(set.contents) || is.null(set.contents))
	    if (!is.null(set.names) && !is.null(set.contents)) {
		stopifnot(length(set.names) == length(set.contents))
		stopifnot(all(!duplicated(set.names)))
	    }
	    
	    if (is.null(set.names)) set.names <- character(0)
	    
	    set.list <- data.frame(ID=character(), Members=character(), stringsAsFactors=F)
	    if (is.null(set.contents)) {
		members <- rep(NA, length(set.names))
	    }else {
		for(i in 1:length(set.names)) {
		    set.list <- SetList.assign(set.list, set.names[i], set.contents[[i]])
		}
	    }
	    return(set.list)
	}

	SetList.assign <- function(sets, id, contents) {
	    if(!(length(contents) == 1 && is.na(contents))) {    
		stopifnot(is.character(contents) && all(!is.na(contents)))
		contents <- unique(contents)        
		contents <- paste(contents, collapse=',')
	    }
	    index <- match(id, sets$ID)
	    if (is.na(index)) {
		sets[nrow(sets)+1,] <- c(id, contents)
	    } else {
		sets$Members[index] <- contents
	    }
	    return(sets)
	}

    sets.component <- function(venn) {
        # extract each part of venndiagram
        sets.output <- SetList.new()
        for(set.indicator in rownames(venn@IndicatorWeight)) {
            set.members <- venn@IntersectionSets[[set.indicator]]
            # Parse set name for output
            set.names.index <- !(colnames(venn@IndicatorWeight) == ".Weight")
            set.names <- colnames(venn@IndicatorWeight)[set.names.index]
            membership.indicator <- venn@IndicatorWeight[set.indicator, set.names]
            output.set.name <- ""
            for(set.name in set.names) {
                if(output.set.name != "")
                        output.set.name <- paste(output.set.name, "_AND_", sep="")
                if(membership.indicator[set.name] == 0)
                    output.set.name <- paste(output.set.name, "NOT_", sep="")
                output.set.name <- paste(output.set.name, set.name, sep="")
            }
            if(is.null(set.members)) set.members <- ""
            sets.output <- SetList.assign(sets.output, output.set.name, set.members)
        }
        sets.output <- as.matrix(sets.output)
        rownames(sets.output) <- sets.output[,1]
        sets.output <- sets.output[,-1,drop=FALSE]
        return(sets.output)
    }
    g.size      <- length(g.list)
    if(g.size <= 1){
        stop('at least two graph should be provided')
    }
    v.attr      <- strsplit(v.attr,split=',')[[1]]
    v.attr.size <- length(v.attr)
    e.attr      <- strsplit(e.attr,split=',')[[1]]
    e.attr.size <- length(e.attr)
    # check whether all given graphs are directed or indirected graphs. They should be the same.
    is.directed    <- tmp.is.directed.graphlist(g.list)
    # check whether given vertex.attr exist in all the graphs
    tmp.attr.check(g.list,v.attr,e.attr)
    # construct a matrix which contains vertices, edges and their corresponding attributes.
    EDGE        <- tmp.prepare.set(g.list,v.attr,e.attr,is.directed=TRUE)
    venn        <- Venn(EDGE)
    vennDrawing <- compute.Venn(venn, type=type,doWeights=doWeights, doEuler=doEuler)
    gpList      <- VennThemes(drawing=vennDrawing,colourAlgorithm=colourAlgorithm)
    pdf(venn.out)
    plot(vennDrawing,gpList=gpList)
    dev.off()
    sets.comp <- sets.component(venn)
    # from each component recontruct each part
    ###############
    names <- rownames(sets.comp)
    sets.comp <- lapply(seq_len(nrow(sets.comp)), function(i) sets.comp[i,])
    names(sets.comp) <- names
    g.out  <- lapply(sets.comp,function(x){
                                    e.list <- strsplit(x,split=',')[[1]]
                                    g.out <- tmp.edgelist2graph(e.list,is.directed=TRUE,v.attr=v.attr,e.attr=e.attr)
                                    return(g.out)
                                    })
    if(is.directed == FALSE){
        g.out   <- lapply(g.out, as.undirected,mode="collapse")
    }
    return(g.out)
}
