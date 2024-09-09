library(testthat)
library(rarestR)
data(share, package = 'rarestR')

Output_tess <- tess(share[1:2,])
Expected_tess <- data.frame(est = 23.28,
                           est.sd = 2.59,
                           model.par = 'logistic')
test_that("Calculate the Total number of Expected Shared Species between two samples", {
  expect_equal(Output_tess$tbl, Expected_tess)
})

