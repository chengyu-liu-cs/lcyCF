lcy.spia <- function(de, all, nB=2000,organism='hsa',pathids=NULL,plots=FALSE,verbose='TRUE',data.dir=NULL,beta=NULL, combine='fisher', output='spa.csv'){
    # data is rownamed matrix. rownames should be ensembl ids, and column(s) corresponding fold changes.
    # SPIA requires 
    # 

    if(missing(de))
        stop('de can not be missing')
    if(missing(all)){
        data(lcyCF)
        all <- LCY.spia.all
    }
    require(SPIA)
    # initial parameters
    PATHWAY_DIAGRAM_URL_PREFIX      <- "http://www.genome.jp/kegg-bin/mark_pathway_www?@"
    PATHWAY_DIAGRAM_FORMAT_GENERAL  <- "/default%3dyellow/"
    PATHWAY_DIAGRAM_UP_FORMAT       <- "%09red,white/"
    PATHWAY_DIAGRAM_DOWN_FORMAT     <- "%09blue,white/"
    # Get pathway data from SPIA package
    data.file <- file.path("extdata",paste(organism,"SPIA.RData",sep=""))
    load(file=system.file(data.file,package="SPIA"))

    # remove NA values and remove multiple id's and only use first one for both deg and reference
    de  <- de[!is.na(de)]
    de  <- de[!duplicated(names(de))]
    # Check that all deg ID's are present in reference
    index <- !names(de) %in% all
    if(sum(index) > 0){
        print(paste(paste(names(de)[index],collapse=','),' are not present in input all', sep=''))
    }
    # run spia
    results <- spia(de,all=all,organism=organism,verbose=verbose,pathids=pathids,plots=plots,data.dir=data.dir,beta=beta,combine=combine)
    
    # Make pathway ids strings for later use. See below.
    results[,2] <- as.character(results[,2])

    # Create custom url:s to pahtway diagrams in kegg. Genes in the deg list
    # are marked to the diagram. Upregulated genes are marked with red
    # and down regulated in blue.
    KEGGurl <- character(nrow(results))
    if (nrow(results)>0){ 
        for(i in 1:nrow(results)) {
            # Add organism code to pathway id
            pathwayID <- paste(organism, results[i,2], sep="")
            KEGGurl[i] <- paste(PATHWAY_DIAGRAM_URL_PREFIX, pathwayID, PATHWAY_DIAGRAM_FORMAT_GENERAL, sep="")
            
            genesInPathway <- rownames(as.data.frame(path.info[names(path.info)==results[i,2]]))
            
            for(gene in genesInPathway) {
                if(gene %in% names(de)) {  
                    fc <- de[match(gene,names(de))]
                    geneName <- paste(organism, ":", gene, sep="")
                    if(fc > 0) {
                        KEGGurl[i] <- paste(KEGGurl[i], geneName, PATHWAY_DIAGRAM_UP_FORMAT, sep="")
                    }else {
                        KEGGurl[i] <- paste(KEGGurl[i], geneName, PATHWAY_DIAGRAM_DOWN_FORMAT, sep="")
                    }
                }
            }
        }
    } 
    # Remove SPIA generated urls and add custom urls to results
    results <- cbind(results[,-12], KEGGurl)
    # Get genes for each pathways
    genes <- character(nrow(results))
    if(length(results[,2])>0){
        genes <- lapply(1:length(results[,2]),function(i){
            genesInPathway  <- rownames(as.data.frame(path.info[names(path.info)==results[i,2]]))
            names           <- names(de)
            genes           <- names[names(de) %in% genesInPathway]
            genes           <- paste(genes,collapse=',')
        })
        genes <- do.call(rbind,genes)
        results <- cbind(results,genes)
    }else{
        results <- cbind(results,genes)
    }
    if(!is.null(output))
        lcy.table.write(file=output,table=results)
    return(results)
}

