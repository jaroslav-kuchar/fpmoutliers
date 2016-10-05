#' MFPOF
#'
#' Feng Lin, Wang Le, Jin Bo - Research on Maximal Frequent Pattern Outlier Factor for Online HighDimensional Time-Series Outlier Detection. Journal of Convergence Information Technology 5(10):66-71. December 2010.
#'
#' @param dataFrame data.frame with input data
#' @param minSupport minimum support for FPM
#' @param mlen maximum length of frequent itemsets
#' @return vector with outlier scores
#' @import arules foreach doParallel parallel
#' @export
MFPOF <- function(dataFrame, minSupport=0.3, mlen=0){
  no_cores <- detectCores() - 1
  registerDoParallel(no_cores)

  dataFrame <- sapply(dataFrame,as.factor)
  dataFrame <- data.frame(dataFrame, check.names=F)
  txns <- as(dataFrame, "transactions")
  if(mlen<=0){
    mlen <- ncol(dataFrame)
  }
  fitemsets <- apriori(txns, parameter = list(support=minSupport, maxlen=mlen, target="frequent itemsets"))
  fitemsets <- fitemsets[which(is.maximal(fitemsets))]

  fiList <- LIST(items(fitemsets))
  qualities <- fitemsets@quality[,"support"]

  scores <- c()
  tx <- NULL
  scores <- foreach(tx = as(txns,"list"), .combine = list, .multicombine = TRUE)  %dopar%  {
    transaction = unlist(tx,"list")
    support <- c()
    for(item in seq(1,length(fitemsets))){
      itemset <- fiList[[item]]
      if(all(itemset %in% transaction)){
        support <- c(support, (qualities[item]*sum((transaction %in% itemset)+0))/ncol(dataFrame))
      }
    }
    sum(support)/length(fitemsets)
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
