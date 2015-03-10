lcy.pathway.search <- function(columns='', database='REACTOME', condition=list(uniProt=NULL, pathwayDesc=NULL, pathwayName=NULL, pathwayId=NULL), type='OR', strsql=""){
    data(lcyCF)
    names <- c('uniProt','pathwayDesc','pathwayName','pathwayId')
    if(strsql != ''){
        df <- sqldf(strsql)
        columns=''
        database=''
        condition <- list(uniProt=NULL, pathwayDesc=NULL, pathwayName=NULL, pathwayId=NULL)
        type= 'OR'
    }else{
        strsql <- 'SELECT'
        if(columns==''){
            columns <- '*'
        }else{
            columns <- strsplit(columns,split=',')[[1]]
            index <- match(columns,names) 
            msg <- paste(paste(columns[is.na(index)],sep=','), ' are not included in the database',sep='')
            columns <- columns[!is.na(index)]
            if(length(columns)< 1){
                columns <- '*'
                warnings('msg')
            }else{
                columns <- paste(columns, collapse=',')
                strsql <- paste(strsql, columns,  collapse=' ')
            }
        }
        index <- match(database, c('PID','REACTOME'))
        if(is.na(index)){
            stop('database provided was wrong. reactome or pid should be provided.')
        }else{
            strsql <- paste(strsql, 'FROM', database, collapse=' ')
        }
        len1 <- length(condition)
        index <- match(names(condition), names)
        condition <- condition[!is.na(index)]
        if(length(condition) < len1){
            msg <- paste('columns ', paste(names(condition)[is.na(index)],sep=','), ' are not included in the database',sep='')
            warnings(msg)
        }
        if(length(condition) < 1){
            condition <- ''
            #strsql <- paste(strsql, condition,collapse=' ')
        }else{
            tmpstrsql <- NULL
            tmp <- lapply(names(condition),function(x) {
                        if(!is.null(condition[[x]])){
                            element <- strsplit(condition[[x]],split=',')[[1]]
                            tmpstrsql <<- c(tmpstrsql, paste(x, element, sep='=',collapse=paste(' ', type, ' ',sep='')))
                        }
                    })
            if(!is.null(tmpstrsql))
                strsql <- paste(strsql, 'WHERE', tmpstrsql, collapse=' ')
        }
    }
    df <- sqldf(strsql)
    return(list(columns=columns,database=database,condition=condition,type=type,strsql=strsql,res=unique(df)))
}


