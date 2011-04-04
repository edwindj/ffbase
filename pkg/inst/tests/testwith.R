library(testthat)
library(ff)

test_that("Chunkify a function",{
	dat <- data.frame(x=1:10, y=10:1) 
	ffdat <- as.ffdf(dat)
   
    z <- with(dat, x+y)
    fz <- with(ffdat, x+y)
     
	expect_identical( z
	                , fz[]
				    )
})