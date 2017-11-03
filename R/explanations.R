#' Plot of a restricted barplot with a highlighted selected value
#'
#' @param data vector with all values
#' @param selectedValue value that will be highlighted
#' @param bars max number of bars to plot, only top lowest and top highest frequencies will be presented
#' @param title main title of the plot
#' @importFrom graphics barplot
#' @export
#' @examples
#' library("fpmoutliers")
#' data <- sample(1:1000, 1000, replace = TRUE)
#' selectedValue <- data[length(data)/2]
#' plotRestrictedBarplot(data, selectedValue, bars = 15,
#'      title=paste("Experimental=",selectedValue,sep=""))
plotRestrictedBarplot <- function(data, selectedValue, bars = 10, title=""){
  frequencies <- sort(table(data))
  selectedValueIndex <- which(names(frequencies)==selectedValue)
  outputIndex <- unique(c(seq(1,min(selectedValueIndex-1,bars/2)),selectedValueIndex, seq(max(length(frequencies)-bars/2+1,selectedValueIndex), length(frequencies))))
  outputFrequencies <- frequencies[outputIndex]
  colors <- rep("gray",length(outputFrequencies))
  colors[which(names(outputFrequencies)==selectedValue)] <- "red"

  spaces=rep(0.1,length(outputFrequencies))
  spaces[which(names(outputFrequencies)==selectedValue)] <- 1.0
  spaces[min(which(names(outputFrequencies)==selectedValue)+1,length(outputFrequencies))] <- 1.0
  barplot(outputFrequencies, col = colors, space = spaces, main = title)
}

#' Visualization of a data instance using a set of barplots
#'
#' @param data data.frame with data describing all instances
#' @param instanceIndex index of the instance to visualize
#' @param bars max number of bars to plot, only top lowest and top highest frequencies will be presented
#' @importFrom graphics par
#' @export
#' @examples
#' library("fpmoutliers")
#' dataFrame <- read.csv(
#'      system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
#' model <- FPI(dataFrame, minSupport = 0.001)
#' # sort data by the anomaly score
#' dataFrame <- dataFrame[order(model$scores, decreasing = TRUE),]
#' visualizeInstance(dataFrame, 1) # instance with the highest anomaly score
#' visualizeInstance(dataFrame, nrow(dataFrame)) # instance with the lowest anomaly score
visualizeInstance <- function(data, instanceIndex, bars=10) {
  anomaly <- data[instanceIndex,]
  cols <- colnames(data)
  par(mfrow=c(ceiling(length(cols)/2),2), mar=c(2,2,1,1))
  for(col in cols){
    plotRestrictedBarplot(data[[col]],anomaly[[col]], bars, title=paste(col,"=",anomaly[[col]],sep=""))
  }
}


#' Explain the instance/outlier by a brief textual summary
#'
#' @param data data.frame with data describing all instances
#' @param model outlier detection model
#' @param instanceIndex index of the instance to visualize
#' @param topN limit for a print of top matching frequent itemsets
#' @export
#' @examples
#' library("fpmoutliers")
#' dataFrame <- read.csv(
#'      system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
#' model <- FPI(dataFrame, minSupport = 0.001)
#' # sort data by the anomaly score
#' dataFrame <- dataFrame[order(model$scores, decreasing = TRUE),]
#' # instance with the highest anomaly score
#' describeInstance(dataFrame, model, 1)
#' # instance with the lowest anomaly score
#' describeInstance(dataFrame, model, nrow(dataFrame))
describeInstance <-function(data, model, instanceIndex, topN = 10){
  output <- list()
  rowIndex <- as.numeric(rownames(data)[instanceIndex])
  cat(paste("Details for the instance:",rowIndex, "\n"))
  output$score <- model$scores[rowIndex]
  cat(paste("* Outlier score:",round(model$scores[rowIndex], 2), "\n"))
  if("partials" %in% names(model)){
    df <- data.frame(
      itemset=names(which(model$partials$coverage[rowIndex,]==1)),
      support=model$model@quality[which(model$partials$coverage[rowIndex,]==1),]$support
    )
    df <- df[order(df$support, decreasing = T), ]
    coverage <- ncol(data)-model$partials$penalization[rowIndex]
    output$coverage <- (coverage/(ncol(data)))
    cat(paste("* Coverage of the instance:", coverage, " of ", ncol(data), "attributes",  "(", (coverage/(ncol(data)))*100, "%)", "\n"))
    cat(paste("* Number of matching frequent itemsets:", nrow(df), "\n"))
    cat(paste("* Frequent itemsets (top-",topN,"):\n",sep=""))
    output$itemsets <- df
    print(head(df, topN))

    # add score for each attribute
    if(is(data,"data.frame")){
      dd <- sapply(data,as.factor)
      dd <- data.frame(dd, check.names=F)
      txns <- as(dd, "transactions")
    } else {
      txns <- data
    }
    # sum od scores of itemsets for each attribute that is member of
    numerator <- model$model@items@data %*% unname(model$partials$scores[rowIndex,])
    # number of itemsets the specific attribute is member of
    denominator <- model$model@items@data %*% model$partials$coverage[rowIndex,]
    # replace zeros to properly comute the fraction/division operation
    denominator[denominator == 0] <- 1
    scores <- t(numerator/denominator)
    # add column names
    colnames(scores) <- model$model@items@itemInfo$labels
    # for penalized attributes - add penalization score
    scores[scores == 0] <- ncol(txns@data)
    # filter by attributes apeared in the data
    scores <- scores[,t(txns@data)[instanceIndex,]]
    cat(paste("* Participations of attributes:\n",sep=""))
    output$scores <- scores
    print(scores)
  }
  output
}
