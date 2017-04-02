## ------------------------------------------------------------------------
# libraries
library(mlbench)
library(fpmoutliers)

# initialize data
data(BreastCancer)
dataFrame <- BreastCancer[2:10]

## ---- results = "hide", message = FALSE----------------------------------
# compute scores and sort
model <- FPI(dataFrame, minSupport = 0.01)
sortedDataFrame <- dataFrame[order(model$scores, decreasing = TRUE),]

## ---- fig.width=7, fig.height=7------------------------------------------
# instance with the highest anomaly score
out <- describeInstance(sortedDataFrame, model, 1)
visualizeInstance(sortedDataFrame, 1)

## ---- fig.width=7, fig.height=7------------------------------------------
# instance with the lowest anomaly score
out <- describeInstance(sortedDataFrame, model, nrow(sortedDataFrame))
visualizeInstance(sortedDataFrame, nrow(sortedDataFrame))

