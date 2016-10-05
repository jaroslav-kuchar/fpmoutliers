#' FPCOF
#'
#' X. Tang, G. Li and G. Chen, "Fast Detecting Outliers over Online Data Streams," 2009 International Conference on Information Engineering and Computer Science, Wuhan, 2009, pp. 1-4.
#'
#' @param dataFrame data.frame with input data
#' @param minSupport minimum support for FPM
#' @param mlen maximum length of frequent itemsets
#' @return vector with outlier scores
#' @import arules foreach doParallel parallel methods
#' @export
FPCOF <- function(dataFrame, minSupport=0.3, mlen=0){
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
    support <- c()
    for(item in seq(1,length(fitemsets))){
      itemset <- fiList[[item]]
      if(all(itemset %in% transaction)){
        support <- c(support, qualities[item]*(length(itemset) - sum((itemset %in% transaction)+0))/length(itemset) )
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
