library(testthat)

test_that("fftable works",{
   b <- factor(rep(c("A","B","C"), 10))
   bf <- ff(b)
   #print(bf)
   fftable(bf)
}) 