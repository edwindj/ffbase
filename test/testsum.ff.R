library(testthat)
library(ff)

test_that("Sum ff works",{
  x <- runif(100)
  fx <- ff(x)
  expect_equal(sum(x), sum(fx))
})