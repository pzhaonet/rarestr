library(testthat)
library(rarestR)
data(share, package = 'rarestR')

Output_tes <- tes(share[1,])
Expected_tes <- data.frame(est = c(138.50, 92.63, 115.56),
                           est.sd = c(2.46, 32.65, 16.37),
                           model.par = c('logistic', 'Weibull', NA))
row.names(Expected_tes) <- c('TESa', 'TESb', 'TESab')

test_that("Calculation of Total Expected Species", {
  expect_equal(Output_tes$tbl, Expected_tes)
})

