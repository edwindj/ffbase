library(testthat)
library(ff)

test_that("Min ff works",{
  x <- runif(100) 
  fx <- ff(x)
  expect_equal(min(x), min(fx))
  
  is.na(x) <- sample(100, 10)
  fx <- ff(x)
  expect_equal(min(x), min(fx))
  expect_equal(min(x, na.rm=TRUE), min(fx, na.rm=TRUE))
})