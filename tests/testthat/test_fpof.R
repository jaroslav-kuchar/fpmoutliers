context('FPOF')

test_that("FPOF", {
  skip_on_cran()
  dataFrame <- read.csv(system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
  model <- FPOF(dataFrame, minSupport = 0.001)
  expect_equal(length(model$model),22)
  expect_equal(model$scores[1],0.1227, tolerance=1e-3)
})

test_that("FPOF 2", {
  skip_on_cran()
  dataFrame <- read.csv(system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
  model <- FPOF(dataFrame, minSupport = 0.001, mlen=1)
  expect_equal(length(model$model),6)
  expect_equal(model$scores[1],0.2667, tolerance=1e-3)
})
