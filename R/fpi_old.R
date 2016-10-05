#' Frequent Pattern Isolation
#'
#' @param dataFrame data.frame with input data
#' @param minSupport minimum support for FPM
#' @param mlen maximum length of frequent itemsets
#' @return vector with outlier scores
#' @import arules foreach doParallel parallel
#' @export
FPI_old <- function(dataFrame, minSupport=0.3, mlen=0){
  no_cores <- detectCores() - 1
  registerDoParallel(no_cores)

  dataFrame <- sapply(dataFrame,as.factor)
  dataFrame <- data.frame(dataFrame, check.names=F)
  txns <- as(dataFrame, "transactions")
  if(mlen<=0){
    mlen <- ncol(dataFrame)
  }
  fitemsets <- apriori(txns, parameter = list(support=minSupport, maxlen=mlen, target="frequent itemsets"))

  fiList <- LIST(items(fitemsets))
  qualities <- fitemsets@quality[,"support"]

  scores <- c()
  tx <- NULL
  scores <- foreach(tx = as(txns,"list"), .combine = list, .multicombine = TRUE)  %dopar%  {
    transaction = unlist(tx,"list")

    coverage <- c()
    support <- c()
    for(item in seq(1,length(fiList))){
      itemset <- fiList[[item]]
      if(all(itemset %in% transaction)){
        support <- c(support, 1/(qualities[item]*length(itemset)) )
        coverage <- unique(c(coverage, itemset))
      }
    }
    # penalization for incomplete coverage
    if(length(coverage)<length(transaction)){
      support <- c(support, rep(nrow(dataFrame),(length(transaction) - length(coverage)) ))
    }

    if(length(support)>0){
      mean(support)
    } else {
      nrow(dataFrame)
    }
  }
  scores <- unlist(scores)
  stopImplicitCluster()
  output <- list()
  output$minSupport <- minSupport
  output$maxlen <- mlen
  output$model <- fitemsets
  output$scores <- scores
  output
}
