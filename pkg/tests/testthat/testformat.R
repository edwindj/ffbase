library(testthat)
library(ff)
library(ffbase)

context("format.ff_vector")

test_that("format accepts positional parameters",{
		  datevec <- rep(as.POSIXct("2016/08/02 18:12:54"), 3)
		  ffdatevec <- ff(datevec)
		  expect_equal(ff(factor(rep("2016", 3))), format(ffdatevec, "%Y"))
})

test_that("format accepts named parametest",{
		  datevec <- rep(as.POSIXct("2016/08/02 18:12:54"), 3)
		  ffdatevec <- ff(datevec)
		  expect_equal(ff(factor(rep("2016", 3))), format(ffdatevec, format="%Y"))
})

