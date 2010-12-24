library(testthat)
library(ff)

test_that("Cut works for ff vectors",{
	x <- 1:10
	xf <- ff(x)
#	expect_identical( cut(x, breaks=3)
#	                , cut(xf, breaks=3)
#				    )
})

test_that("Mean works",{
	x <- runif(100)
	fx <- ff(x)
	expect_equal(mean(x), mean(fx))
})

test_that("Subsetting ff vector works",{
   x <- 1:10
   ss <- x < 5
   fx <- ff(x)
   fss <- ff(ss)
   expect_identical(subset(x, ss), subset(fx,ss)[])   
})

test_that("Subsetting ffdf works",{
})

test_that("Min ff works",{
  x <- runif(100)
  fx <- ff(x)
  expect_equal(min(x), min(fx))
  
  is.na(x) <- sample(100, 10)
  fx <- ff(x)
  expect_equal(min(x), min(fx))
  expect_equal(min(x, na.rm=TRUE), min(fx, na.rm=TRUE))
})

test_that("Range ff works",{
  x <- runif(100)
  fx <- ff(x)
  expect_equal(range(x), range(fx))
  
  is.na(x) <- sample(100, 10)
  fx <- ff(x)
  expect_equal(range(x), range(fx))
  expect_equal(range(x, na.rm=TRUE), range(fx, na.rm=TRUE))
})



test_that("Sum ff works",{
  x <- runif(100)
  fx <- ff(x)
  expect_equal(sum(x), sum(fx))
})
