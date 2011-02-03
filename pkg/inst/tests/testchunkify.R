library(testthat)
library(ff)

test_that("Chunkify a function",{
	x <- 1:10 
	xf <- ff(x)
   
   sin.ff <- chunkify(sin,vmode="double")
   
	expect_identical( sin(x)
	                , sin.ff(xf)[]
				    )
})

test_that("evalInChunks works",{
	
   x <- 1:10
   xf <- ff(x)
   
   dat <- ffdf(xf)
   
   evalInChunks(dat, xf+2)
    
})