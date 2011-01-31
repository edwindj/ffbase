library(testthat)
library(ff)

test_that("risplit works for ff",{
   x <- ff(1:10)
   f <- ff(as.factor(rep(c("M","F"), 5)))
   #risplit(x, f)
})

test_that("risplit works for ffdf",{
   xdf <- as.ffdf(airquality)
   g <- xdf$Month
   #l <- risplit(xdf, g)
})