lcy.DEG.summary <- 
function(data,group=list(treat=c(),ctrl=c()),test.type='ttest',stat.type=c('pvalue','fc','both','individual'),p.adjust.methods='fdr',file=NULL,pairwise=FALSE,...){
###     need to be modified.
    stat.compute <- function(x,group=list(treat=c(),ctrl=c()),test.type='ttest',stat.type=c('pvalue','fc','both','individual'),p.adjust.methods='fdr',...){
        tmp <- list(...)
        test.args <- list(...)
        g1 <- group$treat
        g2 <- group$ctrl
        num.samples <- length(g1)

        switch(test.type,
                ttest={
                    test.function <- t.test 
                },
                #chi.squared={
                #    test.function <- chisq.test
                #},
                #cor.pearson={
                #    test.function <- cor.test
                #    test.args <- list(alternative=alternative, use='pairwise.complete.obs', method='pearson')
                #},
                #cor.spearman={
                #    test.function <- cor.test
                #    test.args <- list(alternative=alternative, use='pairwise.complete.obs', method='spearman')
                #},
                wilcoxon={
                    test.function <- wilcox.test
                },
                {
                    test.function <- t.test 
                })
        if(stat.type=='pvalue'){
            if(length(x[g1][!is.na(x[g1])])>1 & length(x[g2][!is.na(x[g2])])>1 & sd(x[g1],na.rm=TRUE) >= 1e-10 & sd(x[g2],na.rm=TRUE) >= 1e-10){
                y <- t.test(x[g1],x[g2],unlist(tmp))
                deg.summary <- y$p.value
            }else{
                stop("Something wrong when pvalue is computed.\n")
                deg.summary <- NULL
            }
        }else if(stat.type=='fc'){
            deg.summary <- median(x[g1],na.rm=TRUE) - median(x[g2],na.rm=TRUE)
        }else if(stat.type=='both'){
            deg.summary <- matrix(rep(0,2),nr=1)
            if(length(x[g1][!is.na(x[g1])])>1 & length(x[g2][!is.na(x[g2])])>1 & sd(x[g1],na.rm=TRUE) >= 1e-10 & sd(x[g2],na.rm=TRUE) >= 1e-10){
                y <- t.test(x[g1],x[g2],unlist(tmp))
                deg.summary[1,1] <- y$p.value
            }else{
                stop("Something wrong when pvalue is computed.\n")
                deg.summary <- NULL
            }
            deg.summary[2] <- median(x[g1],na.rm=TRUE) - median(x[g2],na.rm=TRUE)
        }else if(stat.type=='individual'){
            deg.summary <- matrix(rep(0,2+num.samples),nr=1)
            if(length(x[g1][!is.na(x[g1])])>1 & length(x[g2][!is.na(x[g2])])>1 & sd(x[g1],na.rm=TRUE) >= 1e-10 & sd(x[g2],na.rm=TRUE) >= 1e-10){
                y <- t.test(x[g1],x[g2],unlist(tmp))
                deg.summary[1,1] <- y$p.value
            }else
                deg.summary[1,1] <- NA
            deg.summary[1,2] <- median(x[g1],na.rm=TRUE) - median(x[g2],na.rm=TRUE)
            for(i in 1:num.samples){
                deg.summary[i+2] <- x[g1[i]] - median(x[g2],na.rm=TRUE)
            }
        }
        return(deg.summary)
    }

    if(missing(data)){
        stop("'data' should be provided.")
    }
    if(length(stat.type)!=1){
        stop("One of 'pvalue','fc','both' are needed 'stat.type'.")
    }
    if(length(group)!=2 && typeof(group)!='list'){
        stop("'group' should be list with two elements.")
    }
    if(!stat.type %in% c('pvalue','fc','both','individual')){
        stop("'stat.type' should be one of 'pvalue','fc','both'.'individual'")
    }
    print('starting\n')
    if(pairwise==FALSE){
        deg.summary <- apply(data,1,stat.compute,group,test.type=test.type,stat.type=stat.type)
        deg.summary <- t(deg.summary)
        switch(stat.type,
                pvalue={
                    col.names <- c('pvalue')
                    deg.summary[,1] <- p.adjust(deg.summary[,1], method=p.adjust.methods)
                },
                fc={
                    col.names <- c('fc')
                    deg.summary <- t(deg.summary)
                },
                both={
                    col.names <- c('pvalue','fc')
                    deg.summary[,1] <- p.adjust(deg.summary[,1], method=p.adjust.methods)
                },
                individual={
                    col.names <- c('pvalue','fc',group$treat)
                    deg.summary[,1] <- p.adjust(deg.summary[,1], method=p.adjust.methods)
                })
        colnames(deg.summary) <- col.names
    }else{
        deg.summary <- apply(data,1,stat.compute,group,test.type=test.type,stat.type='both')
        g1 <- group$treat
        g2 <- group$ctrl
        fc.data         <- lapply(list(1:length(g1)),
                                            function(x){
                                                fc <- data[,g1[x]] - data[,g2[x]]
                                                return(fc)
                                            })
        fc.data         <- fc.data[[1]]
        colnames(fc.data) <- c('pvalue','fc',g1)
    }
    if(!is.null(file))
        lcy.table.write(file=file,table=deg.summary)
    return(deg.summary)
}
