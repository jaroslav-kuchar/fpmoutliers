#' FPCOF algorithm
#'
#' Algorithm proposed by:
#' X. Tang, G. Li and G. Chen, "Fast Detecting Outliers over Online Data Streams," 2009 International Conference on Information Engineering and Computer Science, Wuhan, 2009, pp. 1-4.
#'
#' @param data \code{data.frame} or \code{transactions} from \code{arules} with input data
#' @param minSupport minimum support for FPM
#' @param mlen maximum length of frequent itemsets
#' @param noCores number of cores for parallel computation
#' @return model output (list) with all results including outlier scores
#' @import arules foreach doParallel parallel methods
#' @export
#' @examples
#' library("fpmoutliers")
#' dataFrame <- read.csv(
#'      system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
#' model <- FPCOF(dataFrame, minSupport = 0.001)
FPCOF <- function(data, minSupport=0.3, mlen=0, noCores=1){
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
