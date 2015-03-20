lcy.DEG.samr <- function(x, y, resp.type='Two class unpaired', min.foldchange=2, delta=0.3, nperms=100, nvals=100, logged2=TRUE, censoring.status=NULL, eigengene.number=1, assay.type="array", qvalue=20, pvalue=0.05, outpath='./',genenames=NULL,...){
    require(samr)    
    data            <- list(x=x, y=y, logged2=logged2,
                                censoring.status = censoring.status,
                                geneid=genenames, 
                                genenames=rownames(x), 
                                eigengene.number=eigengene.number)

    samr.obj        <- samr(data, resp.type=resp.type, assay.type=assay.type,random.seed=12345, nperms=nperms)#, ...)
    delta.table     <- samr.compute.delta.table(samr.obj, min.foldchange=min.foldchange, nvals=nvals)
    siggenes.table  <- samr.compute.siggenes.table(samr.obj=samr.obj, del=delta, data=data, delta.table=delta.table, all.genes=TRUE,min.foldchange=min.foldchange)
    a               <- siggenes.table$genes.up; # all up regulated genes
    b               <- siggenes.table$genes.lo; # all down regulated genes
    c               <- rbind(a,b)
    rownames(c)     <- c[,2]
    c               <- c[,-2,drop=FALSE]
    if(is.null(genenames))
        c               <- c[,-2,drop=FALSE]
    pv              <- samr.pvalues.from.perms(samr.obj$tt, samr.obj$ttstar)
    c               <- cbind(c,pvalue=pv[rownames(c)])
    lo              <- c[as.numeric(c[,6]) < qvalue,]
    lo              <- c[as.numeric(c[,7]) < pvalue,]
    if (substr(outpath, start = nchar(outpath), stop = nchar(outpath)) != "/") {
        outpath <- paste(outpath, "/", sep = "")
    }
    write.csv(lo,file=paste(outpath, "DEGs_samr.csv",sep=''))
    return(lo)
}
