lcy.affy.normalization <- function(file.names=character(0),celfile.path=NULL,norm.method='rma',write.file=NULL){
    if(((length(file.names) == 0) && (typeof(file.names) == "character")) && is.null(celfile.path)){
        stop("'file.names' and 'celfile.path' can not be both NULL.")
    }
    require(affy)
    raw.data            <- ReadAffy(filenames=file.names,celfile.path=celfile.path)
    switch(annotation(raw.data),
        hgu133a={
                raw.data@cdfName <- 'hgu133ahsensgcdf'
            },
        hgu133b={
                raw.data@cdfName <- 'hgu133bhsensgcdf'
            },
        hgu133plus2={
                raw.data@cdfName <- 'hgu133plus2hsensgcdf'
            }
        )
    if(norm.method == 'rma'){
        data  <- rma(raw.data)
    }else{
        data    <- mas5(raw.data)
    }
    data    <- log2(exprs(data))
    # remove control probe sets
    tmp             <- regexpr('AFFX',rownames(data),ignore.case=T)
    data            <- data[(tmp!=1),]
    colID           <- colnames(data)
    colID           <- substr(colID,1,nchar(colID)-4)
    colnames(data)  <- colID
    rowID           <- rownames(data)
    rowID           <- substr(rowID,1,nchar(rowID)-3)
    rownames(data)  <- rowID
    if(!is.null(write.file))
         write.table(exprs,file=write.file,quote=FALSE,sep="\t",row.names=TRUE,col.names=TRUE)
    return(data) 
}
