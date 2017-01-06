#' Frequent Pattern Isolation
#'
#' @param data \code{data.frame} or \code{transactions} from \code{arules} with input data
#' @param minSupport minimum support for FPM
#' @param mlen maximum length of frequent itemsets
#' @return vector with outlier scores
#' @import arules foreach doParallel parallel Matrix
#' @export
FPI2 <- function(data, minSupport=0.3, mlen=0){
  # no_cores <- detectCores() - 1
  # registerDoParallel(no_cores)
  # cl <- makeCluster(no_cores)

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

  start.time <- Sys.time()
  # fiLengths <- apply(fitemsets@items@data,2,sum)
  fiLengths <- colSums(fitemsets@items@data)
  end.time <- Sys.time()
  message(paste("FI lengths:", time.taken <- end.time - start.time))

  start.time <- Sys.time()
  fiQualities <- c(t(fitemsets@quality))
  end.time <- Sys.time()
  message(paste("FI qualities:", time.taken <- end.time - start.time))

  start.time <- Sys.time()
  fiCoverages <- Matrix(t(is.subset(fitemsets@items, txns)+0), sparse=TRUE)
  end.time <- Sys.time()
  message(paste("FI coverages:", time.taken <- end.time - start.time))

  start.time <- Sys.time()
  results <-fiCoverages %*% Diagonal(x=1/(fiQualities * fiLengths))
  end.time <- Sys.time()
  message(paste("FI multiply:", time.taken <- end.time - start.time))

  start.time <- Sys.time()
  # TODO: compute sum, add penalizations, divide by length
  # penalization
  # fiC <- apply(txns@data,2,sum) - unname(apply(as(fiCoverages,"ngCMatrix"),1,function(r) sum(apply(fitemsets@items@data[,which(r==1)],1,any))))
  # fiC <- apply(txns@data,2,sum) - unname(apply(as(fiCoverages,"ngCMatrix") %*% t(fitemsets@items@data),1,nnzero))
  fiC <- colSums(txns@data) - unname(rowSums(as(fiCoverages,"ngCMatrix") %*% t(fitemsets@items@data)))
  end.time <- Sys.time()
  message(paste("Penalization:", time.taken <- end.time - start.time))

  start.time <- Sys.time()
  # scores <- unname(apply(results,1, function(x) mean(x[x!=0])))
  # scores <- (unname(apply(results,1, function(x) sum(x[x!=0]))) + fiC*length(txns))/(unname(apply(results,1, function(x) length(x[x!=0]))) + fiC)
  scores <- unname(rowSums(results) + fiC*length(txns))/(unname(apply(results,1, function(x) length(x[x!=0]))) + fiC)
  # scores <- unname(rowSums(results) + fiC*length(txns))/(unname(parApply(cl,results,1, function(x) length(x[x!=0]))) + fiC)
  end.time <- Sys.time()
  message(paste("Means:", time.taken <- end.time - start.time))

  # stopImplicitCluster()
  # stopCluster(cl)
  output <- list()
  output$minSupport <- minSupport
  output$maxlen <- mlen
  output$model <- fitemsets
  output$scores <- scores
  output
}

# file <- system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers")
# data <- read.csv(file)
# dataFrame <- read.csv(system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
# dataFrame <- read.csv(system.file("extdata", "_ESIF_FINANCE_DETAILS.csv", package = "fpmoutliers"))
# FPI2(data)

# t(is.subset(fitemsets@items, txns)+0) %*% diag(c(t(fitemsets@quality)))

# t(is.subset(fitemsets@items, txns)+0) %*% diag(c(t(fitemsets@quality))) %*% diag(apply(fitemsets@items@data,2,sum))


# apply(fiCoverages,1,function(r) fitemsets[which(r==1)]@items@data)
# apply(fiCoverages,1,function(r) r)


