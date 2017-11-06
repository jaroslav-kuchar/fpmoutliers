test_that("PMML parser", {
  skip_on_cran()
  model <- parsePMML("pmml_example.xml")
  expect_equal(model$algorithmName, "FPI")
  expect_equal(model$numberOfOutliers, 150)
  expect_equal(model$minSupport, 0.001)
})

# test_that("PMML builder", {
#   dataFrame <- read.csv(system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
#   model <- FPI(dataFrame, minSupport = 0.001)
#   pmml <- generatePMML(model)
#   expect_equal(unname(xpathSApply(pmml, "//od:OutlierDetectionModel/@algorithmName")),"FPI")
# })
