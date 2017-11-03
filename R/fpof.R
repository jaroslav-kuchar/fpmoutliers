#' FPOF - Frequent Pattern Outlier Factor algorithm
#'
#' Algorithm proposed by:
#' He, Z., Xu, X., Huang, J. Z., Deng, S.: FP-Outlier: Frequent Pattern Based Outlier Detection. Computer Science and Information Systems, Vol. 2, No. 1, 103-118. (2005)
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
#' model <- FPOF(dataFrame, minSupport = 0.001)
FPOF <- function(data, minSupport=0.3, mlen=0, noCores=1){
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
        support <- c(support, qualities[item])
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

#' Frequent Pattern Outlier Factor
#'
#' @param dataFrame data.frame with input data
#' @param anIndex anomaly index
#' @param minSupport minimum support for FPM
#' @param mlen maximum length of frequent itemsets
#' @param k top-k contradictness
#' @return vector with outlier scores
#' @import arules foreach doParallel parallel
#' @export
FPOFcontradictness <- function(dataFrame, anIndex, minSupport=0.3, mlen=0, k = 10){
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

  # scores <- foreach(tx = as(txns,"list"), .combine = list, .multicombine = TRUE)  %dopar%  {
  contradict <- list()

  for(cc in anIndex){
    transaction = unlist(as(txns,"list")[cc])
    support <- c()
    for(item in seq(1,length(fitemsets))){
      itemset <- fiList[[item]]
      if(all(itemset %in% transaction)==FALSE){
        support <- c(support, (length(itemset) - sum((transaction %in% itemset)+0))*qualities[item])
      } else {
        support <- c(support, 0.0)
      }
    }
    n <- length(support)
    mm <- sort(support,partial=n-k)[n-k]
    out <- c()
    for(a in which(support>=mm)){
      out <- c(out, paste(paste(fiList[[a]], collapse=","),"(",qualities[a],")",sep=""))
    }
    contradict[[as.character(cc)]] <- out
  }
  stopImplicitCluster()
  contradict
}
