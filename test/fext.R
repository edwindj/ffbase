library(testthat)

test_that("Cut works for ff vectors",{
	x <- 1:10
	xf <- ff(x)
	print(cut(xf,3))
	expect_identical( cut(x, breaks=3)
	                , cut(xf, breaks=3)
				    )
})

