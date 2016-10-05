#' LFPOF
#'
#' W. Zhang, J. Wu and J. Yu, "An Improved Method of Outlier Detection Based on Frequent Pattern," Information Engineering (ICIE), 2010 WASE International Conference on, Beidaihe, Hebei, 2010, pp. 3-6.
#'
#' @param dataFrame data.frame with input data
#' @param minSupport minimum support for FPM
#' @param mlen maximum length of frequent itemsets
#' @return vector with outlier scores
#' @import arules foreach doParallel parallel
#' @export
LFPOF <- function(dataFrame, minSupport=0.3, mlen=0){
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
    metric <- c()
    for(item in seq(1,length(fitemsets))){
      itemset <- fiList[[item]]
      if(all(itemset %in% transaction)){
        metric <- c(metric, length(itemset))
      }
    }
    max(metric)/length(transaction)
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
