#' PMML conversion - PMML representation of outliers
#'
#' Kuchar, Jaroslav et al. “Outlier (Anomaly) Detection Modelling in PMML.” RuleML+RR (2017).(http://ceur-ws.org/Vol-1875/paper9.pdf)
#'
#' @param model outlier model
#' @param dataFrame frame for labeling
#' @param topN limit number of outliers in the output
#' @return pmml model
#' @import pmml XML arules
#' @export
#' @examples
#' \dontrun{
#' library("fpmoutliers")
#' dataFrame <- read.csv(
#'      system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
#' model <- FPI(dataFrame, minSupport = 0.001)
#' generatePMML(model, dataFrame)
#' }
generatePMML <- function(model, dataFrame=NULL, topN=NULL){

  odNS <- "http://www.example.com/od"

  mainNode <- xmlNode("PMML", attrs=c(version="4.3", xmlns="http://www.dmg.org/PMML-4_3",
                          "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance",
                          "xmlns:od"=odNS,
                          "xsi:schemaLocation"=paste("http://www.dmg.org/PMML-4_3", "pmml-4-3+od-0-1.xsd")))

  header <- xmlNode("Header", attrs=c(copyright="", description=""))
  header <- append.XMLNode(header, xmlNode("Timestamp", sprintf("%s", Sys.time())))
  mainNode <- append.XMLNode(mainNode, header)

  if(is.null(topN) || topN<1 || topN>length(model$scores)){
    topN <- length(model$scores)
  }

  odModel <- xmlNode("od:OutlierDetectionModel", attrs=c("xmlns"=odNS, "algorithmName"="FPI", "modelName"="FPI OD model", "typeOfOutliers"="point", "numberOfOutliers"=topN, "output"="score"))

  # MiningSchema
  miningSchema <- xmlNode("MiningSchema", attrs = c("xmlns"="http://www.dmg.org/PMML-4_3"))
  odModel <- append.XMLNode(odModel, miningSchema)

  parameterList <- xmlNode("ParameterList")
  parameterList <- append.XMLNode(parameterList, xmlNode("Parameter", attrs=c("name"="minSupport", "value"=model$minSupport)))
  odModel <- append.XMLNode(odModel, parameterList)

  amPmml <- pmml.itemsets(model$model)
  associationModel <- amPmml$children[[3]]
  associationModel <- addAttributes(associationModel, "xmlns"=as.character(amPmml$attributes["xmlns"]))

  odModel <- append.XMLNode(odModel, associationModel)

  if(!is.null(dataFrame)){
    labeledInstances <- xmlNode("LabeledInstances")
    # InstanceFields
    inlineTable <- xmlNode("InlineTable")

    dataFrame$pmml_od_scores <- model$scores
    dataFrame$pmml_od_index <- row.names(dataFrame)

    dataFrame <- dataFrame[order(dataFrame$pmml_od_scores, decreasing = TRUE),]
    dataFrame <- dataFrame[1:topN,]

    rows <- apply(dataFrame,1, function(rr) {
      rowNode <- xmlNode("Row", attrs=c("id"=as.character(rr["pmml_od_index"]), "output"=as.character(rr["pmml_od_scores"])))
      # id attribute
      for(name in names(dataFrame)){
        if(name != "pmml_od_scores" && name != "pmml_od_index"){
          rowMem <- xmlNode(as.character(name), rr[name])
          rowNode <- append.XMLNode(rowNode, rowMem)
        }
      }
      rowNode
      })
    rows
    inlineTable <- append.XMLNode(inlineTable, rows)

    labeledInstances <- append.XMLNode(labeledInstances, inlineTable)

    odModel <- append.XMLNode(odModel, labeledInstances)
  }

  mainNode <- append.XMLNode(mainNode, odModel)
  mainNode
}

#' PMML parser
#'
#' The parser parses the proposed PMML for the outlier detection model and build its object representation.
#'
#' @param fileName xml file name
#' @return list model
#' @import pmml XML
#' @export
#' @examples
#' \dontrun{
#' library("fpmoutliers")
#' model <- parsePMML("od-pmml.xml")
#' }
parsePMML <- function(fileName) {
  doc <- xmlTreeParse(fileName, useInternalNodes = TRUE)
  top <- xmlRoot(doc)
  output <- list()

  if(length(xpathSApply(top, "//od:OutlierDetectionModel"))>0){
    output$numberOfOutliers <- as.numeric(unname(xpathSApply(top, "//od:OutlierDetectionModel/@numberOfOutliers")))
    output$algorithmName <- unname(xpathSApply(top, "//od:OutlierDetectionModel/@algorithmName"))
    output$minSupport <- as.numeric(unname(xpathSApply(top, "//od:OutlierDetectionModel/od:ParameterList/od:Parameter[@name='minSupport']/@value")))
  } else {
    stop("Element od:OutlierDetectionModel does not exist!")
  }
  output
}
