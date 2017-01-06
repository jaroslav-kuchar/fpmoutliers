#' Frequent Pattern Isolation
#'
#' @param data \code{data.frame} or \code{transactions} from \code{arules} with input data
#' @param minSupport minimum support for FPM
#' @param mlen maximum length of frequent itemsets
#' @return vector with outlier scores
#' @import arules foreach doParallel parallel
#' @export
FPI_old <- function(data, minSupport=0.3, mlen=0){
  no_cores <- detectCores() - 1
  registerDoParallel(no_cores)

  if(is(data,"data.frame")){
    data <- sapply(data,as.factor)
    data <- data.frame(data, check.names=F)
    txns <- as(data, "transactions")
  } else {
    txns <- data
  }
  if(mlen<=0){
    mlen <- length(unique(txns@itemInfo$variables))
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
      support <- c(support, rep(length(txns),(length(transaction) - length(coverage)) ))
    }

    if(length(support)>0){
      mean(support)
    } else {
      length(txns)
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
