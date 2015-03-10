lcy.ihc.grouping <-
function(data, col.er='ER', col.pr='PR',col.her2='HER2',val.pos='Positive',val.neg='Negative', file='brcaIHC.csv'){

    tnbc.index  <- (data[,col.er] == val.neg) & (data[,col.pr] == val.neg) & (data[,col.her2] == val.neg)
    her2.index  <- (data[,col.er] == val.neg) & (data[,col.pr] == val.neg) & (data[,col.her2] == val.pos)
    lum1.index  <-((data[,col.er] == val.pos) | (data[,col.pr] == val.pos))& (data[,col.her2] == val.neg)
    lum2.index  <-((data[,col.er] == val.pos) | (data[,col.pr] == val.pos))& (data[,col.her2] == val.pos)
    tpbc.index  <- (data[,col.er] == val.pos) & (data[,col.pr] == val.pos) & (data[,col.her2] == val.pos)

    colnames    <- rownames(data)
    tnbc        <- colnames[tnbc.index]
    her2        <- colnames[her2.index]
    lum1        <- colnames[lum1.index]
    lum2        <- colnames[lum2.index]
    tpbc        <- colnames[tpbc.index]

    if(!is.null(file)){
        table <- data.frame(group=c('tnbc','her2','lum1','lum2','tpbc'),
                            sample=c(paste(tnbc,collapse=','), paste(her2,collapse=','),paste(lum1,collapse=','),paste(lum2,collapse=','),paste(tpbc,collapse=',')))
        lcy.table.write(file=file, table=table)
    }
    return(list(tnbc=tnbc,her2=her2,lum1=lum1,lum2=lum2,tpbc=tpbc))
}

