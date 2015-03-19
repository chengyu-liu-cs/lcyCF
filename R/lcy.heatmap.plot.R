lcy.heatmap.plot <-
function(data,output='heatmap.pdf',col='greenred',group=NULL,symbreaks=TRUE,density.info="none",cexRow=0.5,cexCol=0.3,width=16.5,height=12,keysize=0.5,...){

    color.map <- function(group, labels){
    # re-order group based on labels, in order that their order is the same as labels which are colnames or rownames or data.
            label.colors        <- c('red3','green','blue','yellow','purple','pink','sienna','lightseagreen')
            rownames(group)     <- group[,1]
            unique              <- unique(group[,2])
            legend.color        <<-label.colors[1:length(unique)]
            legend.label        <<-as.character(unique)
            if(length(unique) >= length(label.colors))
                stop('two many classes in group.')
            group[,2]           <- label.colors[lcy.in(group[,2],list=unique)[,2]]
            if(missing(labels) | is.null(labels))
                labCol          <- group[,2]
            labCol              <- group[labels,2]
            index               <- is.na(labCol)
            labCol[index]       <- 'black'
            return(labCol)
    }
    require(gplots)
    pdf(output,width=width, height=height)
    tryCatch({
    # catch all in this block. mainly for the issue when you open one file connection, you can not close it if there ar error when you plot.
    #switch(col, 
    #    redgreen={  col=greenred(75)
    #    },
    #    bluegreen={ col=topo.colors(75)
    #    },
    #    bluered={   col=bluered(75)
    #    },
    #    heat={      col = "heat.colors"
    #    }
    #)
    
    if(missing(data))
        stop('data is missing')
    if(missing(group) | is.null(group)){
        print('Without group')
        ht.obj <- heatmap.2(data, col=col,trace="none",symbreaks=symbreaks,density.info=density.info,cexRow=cexRow,cexCol=cexCol,...)
    }else{
        legend.color <- c()
        legend.label <- c()
        legend.colorR <- c()
        legend.labelR <- c()
        legend.colorC <- c()
        legend.labelC <- c()
        rownames <- rownames(data)
        colnames <- colnames(data)
        names    <- attributes(group)
        if(length(group)== 2){
            print('With group which has col and row')
            ColSideColors <- color.map(group$col,colnames)
            legend.colorC <- legend.color
            legend.labelC <- legend.label
            RowSideColors <- color.map(group$row,rownames) 
            legend.colorR <- legend.color
            legend.labelR <- legend.label
            ht.obj <- heatmap.2(data, col=col,ColSideColors=ColSideColors,RowSideColors=RowSideColors,trace="none",symbreaks=symbreaks,density.info=density.info,cexRow=cexRow,cexCol=cexCol,...)
            legend("topright",legend=legend.labelC,fill=legend.colorC,cex=0.5,text.col='white',title='Column color')
            legend("bottomright",legend=legend.labelR,fill=legend.colorR,cex=0.5,text.col='white',title='Row color')
        }else{
            switch(attr(group, 'names'),
                col={
                    print('Only ColSideColors is available')
                    ColSideColors <- color.map(group$col,colnames)
                    legend.colorC <- legend.color
                    legend.labelC <- legend.label
                    ht.obj <- heatmap.2(data,col=col,ColSideColors=ColSideColors,trace="none",symbreaks=symbreaks,density.info=density.info,cexRow=cexRow,cexCol=cexCol,...)
                    legend(0.9,0.1,legend=legend.labelC, cex=0.5,fill=legend.colorC, text.col='white',title='Column color')
                },row={
                    print('Only RowSideColors is available')
                    RowSideColors <- color.map(group$rown,rownames) 
                    legend.colorR <- legend.color
                    legend.labelR <- legend.label
                    ht.obj <- heatmap.2(data, col=col,RowSideColors=RowSideColors,trace="none",symbreaks=symbreaks,density.info=density.info,cexRow=cexRow,cexCol=cexCol,...)
                    legend("bottomright",   col=legend.colorR,legend=legend.labelR,cex=0.5,text.col='white',title='Row color')
                },
                    stop('group should have col or row')
            )
        }
    }
    },error=function(e) {
        print(e)
        ht.obj <-list()
    })
    dev.off()
    return(ht.obj)
}


lcy.aheatmap <- function(data, clinic, col='RdYlBu', scale='row', annCol, distfun=function(x) as.dist((1-cor(t(x))/2)), Rowv = "correlation", Colv="man", hclustfun='ward', main="heatmap", fontsize=7){
    if(missing(annCol)){
        annCol <- NULL
    }
    if(!missing(clinic)){
        samples         <- intersect(colnames(data), rownames(clinic))
        label           <- rep(NA, ncol(data))
        names(label)    <- colnames(data)
        tmp             <- apply(clinic, 2, function(x) {
                                label.loc   <- label
                                label.loc[samples] <- x[samples]
                                return(label.loc)
                            })
        annCol          <- lapply(1:ncol(tmp), function(x) {tmp[,x]})
        names(annCol)   <- colnames(clinic)
    }
    colors              <- rainbow(9)
    if(length(annCol)>=0){
        annColors       <- lapply(1:length(annCol), function(x) {
                                if(length(unique(annCol[[x]])) <= length(colors)){
                                    colors[1:length(table(annCol[[x]]))]
                                }else{
                                    #c('red','blue')
                                    NULL
                                }
                            })
    }
    if(ncol(data)> 1){
        h <- aheatmap(data, col='RdYlBu',scale='row',annCol=annCol, distfun=distfun, Rowv=Rowv, Colv=Colv, 
                        hclustfun=function(y) hclust(y,method='ward'), main=main, fontsize=fontsize, annColors=annColors)
    }
    return(h)
}
