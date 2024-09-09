library(testthat)
library(rarestR)
data(share, package = 'rarestR')

esa <- es(share, m = 100)
names(esa) <- NULL
test_that("Calculate the Expected Species with method a", {
  expect_equal(round(esa, 4), round(c(58, 47.77653, 53.00568), 4))
})

esb <- es(share, method = "b", m = 100)
names(esb) <- NULL
test_that("Calculate the Expected Species", {
  expect_equal(round(esb, 4), round(c(43.51041, 40.74378, 46.19118), 4))
})

# esa2 <- es(share, m = 150)
# names(esa2) <- NULL
# test_that("Calculate the Expected Species with method b", {
#   expect_equal(round(esa2, 4), round(c(NA, 57, 65.2415), 4))
# })
