library(testthat)

test_that("fftabulate works",{
   x <- c(1,1,2,3,3)
   xf <- ff(x)
   print(str(tabulate(x,2)))
   print(str(fftabulate(xf,2)))
   
   expect_identical(tabulate(x), fftabulate(xf))
   
   expect_identical(tabulate(x,2), fftabulate(xf,2))
   
   expect_identical(tabulate(x,2), fftabulate(xf,2, TRUE)[])
})