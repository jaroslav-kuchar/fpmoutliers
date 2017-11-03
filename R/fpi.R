#' FPI - Frequent Pattern Isolation algorithm
#'
#' Algorithm proposed by:
#' J. Kuchar, V. Svatek: Spotlighting Anomalies using Frequent Patterns, Proceedings of the KDD 2017 Workshop on Anomaly Detection in Finance, Halifax, Nova Scotia, Canada, PMLR, 2017.
#'
#' @param data \code{data.frame} or \code{transactions} from \code{arules} with input data
#' @param minSupport minimum support for FPM
#' @param mlen maximum length of frequent itemsets
#' @return model output (list) with all results including outlier scores
#' @import arules foreach doParallel parallel Matrix
#' @importFrom pryr mem_used
#' @export
#' @examples
#' library("fpmoutliers")
#' dataFrame <- read.csv(
#'      system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
#' model <- FPI(dataFrame, minSupport = 0.001)
FPI <- function(data, minSupport=0.3, mlen=0){

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
  # mine frequent itemsets
  fitemsets <- apriori(txns, parameter = list(support=minSupport, maxlen=mlen, target="frequent itemsets"))

  # compute lengths of individual frequent itemsets
  start.time <- Sys.time()
  fiLengths <- colSums(fitemsets@items@data)
  end.time <- Sys.time()
  message(paste("FI lengths:", time.taken <- end.time - start.time, "(", mem_used(), ")"))

  # extract support of individual frequent itemsets
  start.time <- Sys.time()
  fiQualities <- c(t(fitemsets@quality$support))
  end.time <- Sys.time()
  message(paste("FI qualities:", time.taken <- end.time - start.time, "(", pryr::mem_used(), ")"))

  # compute which itemset covers which transaction and provide as a matrix itemsets X transactions
  start.time <- Sys.time()
  fiCoverages <- Matrix(t(is.subset(fitemsets@items, txns)+0), sparse=TRUE)
  end.time <- Sys.time()
  message(paste("FI coverages:", time.taken <- end.time - start.time, "(", pryr::mem_used(), ")"))

  # compute basic score for each coverage -> itemsets X transactions
  start.time <- Sys.time()
  results <-fiCoverages %*% Diagonal(x=1/(fiQualities * fiLengths))
  end.time <- Sys.time()
  message(paste("FI multiply:", time.taken <- end.time - start.time, "(", pryr::mem_used(), ")"))

  # compute how many items of each transaction is not covered by appropriate frequent itemsets
  start.time <- Sys.time()
  fiC <- colSums(txns@data) - unname(rowSums(as(fiCoverages,"ngCMatrix") %*% t(fitemsets@items@data)))
  end.time <- Sys.time()
  message(paste("Penalization:", time.taken <- end.time - start.time, "(", pryr::mem_used(), ")"))

  # compute final score as a mean value of scores and penalizations: (sum of scores + penalization*number of transactions)/(number of scores + penalization)
  # penalization with number of transactions is the worse case, the lowest support only one instance
  start.time <- Sys.time()
  scores <- unname(rowSums(results) + fiC*length(txns))/(unname(apply(results,1, function(x) length(x[x!=0]))) + fiC)
  end.time <- Sys.time()
  message(paste("Means:", time.taken <- end.time - start.time, "(", pryr::mem_used(), ")"))

  output <- list()
  output$minSupport <- minSupport
  output$maxlen <- mlen
  output$model <- fitemsets
  output$scores <- scores
  output$txns <- txns
  output$partials <- list(
    coverage = fiCoverages,
    scores = results,
    penalization = fiC
  )
  # scores
  output
}





