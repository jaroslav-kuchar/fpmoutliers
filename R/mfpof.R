#' MFPOF algorithm
#'
#' Algorithm proposed by:
#' Feng Lin, Wang Le, Jin Bo - Research on Maximal Frequent Pattern Outlier Factor for Online HighDimensional Time-Series Outlier Detection. Journal of Convergence Information Technology 5(10):66-71. December 2010.
#'
#' @param data \code{data.frame} or \code{transactions} from \code{arules} with input data
#' @param minSupport minimum support for FPM
#' @param mlen maximum length of frequent itemsets
#' @param noCores number of cores for parallel computation
#' @return model output (list) with all results including outlier scores
#' @import arules foreach doParallel parallel
#' @export
#' @examples
#' library("fpmoutliers")
#' dataFrame <- read.csv(
#'      system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
#' model <- MFPOF(dataFrame, minSupport = 0.001)
MFPOF <- function(data, minSupport=0.3, mlen=0, noCores=1){
  registerDoParallel(noCores)

  if(is(data,"data.frame")){
    data <- sapply(data,as.factor)
    data <- data.frame(data, check.names=F)
    txns <- as(data, "transactions")
  } else {
    txns <- data
  }
  if(mlen<=0){
    variables <- unname(sapply(txns@itemInfo$labels,function(x) strsplit(x,"=")[[1]][1]))
    mlen <- length(unique(variables))
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
        support <- c(support, (qualities[item]*sum((transaction %in% itemset)+0))/length(unique(txns@itemInfo$variables)))
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
