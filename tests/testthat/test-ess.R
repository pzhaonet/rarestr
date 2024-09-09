library(testthat)
library(rarestR)
data(share, package = 'rarestR')

ess1 <- ess(share)
test_that("Calculate the dissimilarity estimates between two samples with m = 1, index = 'CNESSa'", {
  expect_equal(round(c(ess1), 4), c(0.7971, 0.6360, 0.7642))
})


ess2 <- ess(share, m = 100)
test_that("Calculate the dissimilarity estimates between two samples with m = 100, index = 'CNESSa'", {
  expect_equal(round(c(ess2), 4), c(0.8567,0.7308,0.8229))
})

ess3 <- ess(share, m = 100, index = "ESS")
test_that("Calculate the dissimilarity estimates between two samples with m = 100, index = 'ESS'", {
  expect_equal(round(c(ess3), 4), c(13.0174,22.6567,13.2392))
})
