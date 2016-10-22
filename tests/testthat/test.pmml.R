test_that("PMML parser", {
  model <- parsePMML("pmml_example.xml")
  expect_equal(model$algorithmName, "FPI")
  expect_equal(model$numberOfOutliers, 150)
  expect_equal(model$minSupport, 0.001)
})
