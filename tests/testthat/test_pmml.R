test_that("PMML parser", {
  skip_on_cran()
  model <- parsePMML("pmml_example.xml")
  expect_equal(model$algorithmName, "FPI")
  expect_equal(model$numberOfOutliers, 150)
  expect_equal(model$minSupport, 0.001)
})

test_that("PMML builder", {
  skip_on_cran()
  library(arules)
  dataFrame <- read.csv(system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
  model <- FPI(dataFrame, minSupport = 0.001)
  pmml <- generatePMML(model)
  # expect_equal(unname(xpathSApply(pmml, "//OutlierDetectionModel/@algorithmName", namespaces = "od")),"FPI")
  expect_equal(unlist(getNodeSet(pmml, "//od:OutlierDetectionModel", namespaces = c(od="http://www.example.com/od")))[["attributes.algorithmName"]],"FPI")
})
