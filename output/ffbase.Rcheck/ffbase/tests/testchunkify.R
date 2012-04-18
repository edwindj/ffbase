library(testthat)
library(ff)

context("chunkify")

test_that("Chunkify a function",{
	x <- 1:10 
	xf <- ff(x)
   
  sin.ff <- chunkify(sin)
   
	expect_identical( sin(x)
	                , sin.ff(xf)[]
				    )
})
