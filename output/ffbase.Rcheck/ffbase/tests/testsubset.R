library(testthat)
library(ff)

context("subset")

test_that("Subsetting ff vector works",{
   x <- 1:10
   ss <- x < 5
   fx <- ff(x)
   fss <- ff(ss)
   expect_identical(subset(x, ss), subset(fx,ss)[])   
})

test_that("Subsetting ffdf works",{
})