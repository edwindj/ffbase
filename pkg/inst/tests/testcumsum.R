library(testthat)
library(ff)

context("cumsum, cumprod, cummax, cummin")

test_that("cumsum works",{
	x <- rnorm(100000)
	test.ram <- cumsum(x)
	test.ff <- cumsum.ff(ff(x), by=1000)

	expect_equal( test.ram
	                , test.ff[]
				    )
})

test_that("cummax works",{
	x <- rnorm(100000)
	test.ram <- cummax(x)
	test.ff <- cummax.ff(ff(x), by=1000)

	expect_equal( test.ram
	                , test.ff[]
				    )
})

test_that("cummin works",{
	x <- rnorm(100000)
	test.ram <- cummin(x)
	test.ff <- cummin.ff(ff(x), by=1000)

	expect_equal( test.ram
	                , test.ff[]
				    )
})

test_that("cumprod works",{
	x <- rnorm(100000)
	test.ram <- cumprod(x)
	test.ff <- cumprod.ff(ff(x), by=1000)

	expect_equal( test.ram
	                , test.ff[]
				    )
})


