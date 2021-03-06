lcy.classifier <- function(data, label, k=0, classifier='naiveBayes'){
    uni.label <- unique(label)
    num <- c()
    x <- matrix(numeric(0), nr=nrow(data))
    y   <- c()
    for(i in 1:length(uni.label)){
        num    <- c(num,sum(label == uni.label[i]))
        x    <- cbind(x, data[,label == uni.label[i],drop=FALSE])
        y   <- c(y,rep(as.character(uni.label[i]),num[i]))
    }
    return.value <- list()
    if (k <= 1){ # independent validation
        ncol            <- ncol(x)
        split           <- 1:ncol
        train           <- x[,split %in% seq(1,ncol,2)]
        train.label     <- y[split %in% seq(1,ncol,2)]
        testing         <- x[,split %in% seq(2,ncol,2)]
        testing.label   <- y[split %in% seq(2,ncol,2)]
        switch(classifier,
            naiveBayes={
                cf              <-naiveBayes(t(train), train.label)
                ypredscore      <- predict(cf, t(testing), type='raw')
            },
            svm={
                cf              <- ksvm(t(train), train.label, type="C-svc", kernel='rbf')
                ypredscore      <- predict(cf, t(testing), type="decision")

            }
        )
        ## if we want to support multi-label classification, I need to think of how to ROC plot multi-label. 
        ## Example in http://stackoverflow.com/questions/11467855/roc-curve-in-r-using-rocr-package
        return.value$prediction <- ypredscore
        return.value$label      <- testing.label
    }else{
        # k-fold cross validation
        split           <- 1:ncol
        sapply(1:k, function(i){
                                    test.indx       <- splits %in% seq(i,ncol,k)
                                    train.indx      <- !test.indx
                                    testing         <- x[,test.indx,drop=FALSE]
                                    testing.label   <- y[test.indx]
                                    train           <- x[,train.indx,drop=FALSE]
                                    train.label     <- y[train.indx]
                                    switch(classifier,
                                        naiveBayes={
                                            cf              <-naiveBayes(t(train), train.label)
                                            ypredscore      <- predict(cf, t(testing), type='raw')
                                        },
                                        svm={
                                            cf              <- ksvm(t(train), train.label, type="C-svc", kernel='rbf')
                                            ypredscore      <- predict(cf, t(testing), type="decision")

                                        }
                                    )
                                    return.value$prediction[[i]]    <<- ypredscore
                                    return.value$label[[i]]         <<- testing.label
                                })
    }
    return(return.value)
}

lcy.pam.predict <- function (data, fit, postRth = 1){
    require(pamr)
    signature       <- fit$signature
    pam.rslt        <- fit$pam.rslt
    thresh          <- fit$thresh

    if(is.null(colnames(data))){
        colnames(data) <- paste('col',1:ncol(data),sep='_')
    }
    pred            <- pamr.predict(pam.rslt, data, thresh, type = "posterior")
    maxr            <- apply(pred, 1, max)
    postR           <- maxr/(1 - maxr)
    sel.samples.f   <- names(postR)[which(postR >= postRth)]
    clu.pred        <- apply(pred, 1, which.max)
    #clu.pred        <- sort(clu.pred)
    #clu.pred.reord  <- NULL
    #for (cl in 1:ncol(pred)) {
    #    temp            <- names(sort(postR[names(clu.pred[clu.pred == cl])]))
    #    clu.pred.reord  <- c(clu.pred.reord, temp)
    #}
    #clu.pred    <- clu.pred[clu.pred.reord]
    #nam.ord     <- names(clu.pred)
    #sdat.sig    <- data[signature, nam.ord]
    #gclu.f      <- hclust(as.dist(1 - cor(t(sdat.sig))), method = "complete") 
    #return(list(sdat.sig=sdat.sig, pred=pred, clu.pred=clu.pred, nam.ord=nam.ord, gclu.f=gclu.f))
    attr(pred, "scaled:scale") <- NULL
    return(list(class=clu.pred, prob=pred))
}


lcy.pam.train <- function(data, label, nfold = 10, nboot = 100, err.cutoff=0.02, n.threshold = 30, err.pdf=TRUE, thresh, seed=123456){
    # n.threshold = 30 for pamr.train : Number of threshold values desired (default 30)
    # if there is any threshold which smaller than err.cutoff, then all genes are considered as signature.
    getCentroids <- function (fit, data, threshold) {
        genenames <- data$genenames[fit$gene.subset]
        x <- data$x[fit$gene.subset, fit$sample.subset]
        clabs <- colnames(fit$centroids)
        scen <- pamr.predict(fit, data$x, threshold = threshold, 
            type = "cent")
        dif <- (scen - fit$centroid.overall)/fit$sd
        if (!is.null(fit$y)) {
            nc <- length(unique(fit$y))
        }
        if (is.null(fit$y)) {
            nc <- ncol(fit$proby)
        }
        o <- drop(abs(dif) %*% rep(1, nc)) > 0
        d <- dif[o, ]
        nd <- sum(o)
        genenames <- genenames[o]
        xx <- x[o, ]
        oo <- order(apply(abs(d), 1, max))
        d <- d[oo, ]
        genenames <- genenames[oo]
        return(d)
    }
    require(pamr)
    set.seed(seed)
    dat <- list()
    if(is.null(names(label))){
        names <- 1: length(label)
    }
    if(is.null(rownames(data))){
        rownames(data) <- paste("row",1:nrow(data),sep='_')
    }
    dat$x <- data[, names]
    dat$y <- label
    dat$y <- factor(dat$y)
    dat$geneid      <- rownames(dat$x)
    dat$genenames   <- rownames(dat$x)
    pam.rslt        <- pamr.train(data = dat,n.threshold=n.threshold)
    pam.cv.rslt.l   <- list()
    for (i in 1:nboot) {
        pam.cv.rslt         <- pamr.cv(fit = pam.rslt, data = dat, nfold = nfold)
        pam.cv.rslt.l[[i]]  <- pam.cv.rslt
    }
    err             <- t(sapply(1:length(pam.cv.rslt.l), function(x) pam.cv.rslt.l[[x]]$error, simplify = TRUE))
    ngenes          <- c(sapply(1:(length(pam.rslt$threshold) - 1), function(x) nrow(pamr.listgenes(pam.rslt, dat, pam.rslt$threshold[x]))), 0)
    colnames(err)   <- ngenes
    # choose threshold cutoff. select the first index where error rate is smaller than err.cutoff.
    if(err.pdf){
        pdf("Pamr_error_rate.pdf")
        boxplot(err, xlab = "No. of genes", ylab = "error rate")
        dev.off()
    }
    index           <- which(colMeans(err) <= err.cutoff)
    if(length(index)<=0){
        index <- 1
    }else{
        index <- index[length(index)]
    }
    print(pam.rslt$threshold)
    if(missing(thresh)){
        thresh      <- pam.rslt$threshold[index]
    }else{
        thresh      <- pam.rslt$threshold[n.threshold - thresh + 1]
    }
    signature       <- (pamr.listgenes(pam.rslt, dat, thresh))[, "id"]
    print(length(signature))
    cents           <- getCentroids(pam.rslt, dat, thresh)
    cents           <- cents[signature, ]
    print("pam is done")
    return(list(signature = signature, pam.rslt = pam.rslt, thresh = thresh, err = err, cents = cents))
}
