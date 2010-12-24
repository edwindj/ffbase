library(testthat)
library(ff)

test_that("ffsplit works",{
   x <- ff(1:10)
   f <- ff(as.factor(rep(c("M","F"), 5)))
   split(x, f)
})