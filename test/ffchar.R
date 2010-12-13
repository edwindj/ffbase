library(testthat)

test_that("creation of ffchar works",{
	cvec <- c("Hello", "", "World!", "", "?")
	fc <- ffchar(cvec)
	expect_equal(length(fc), length(cvec))
})

test_that("retrieving is ok",{
	cvec <- c("Hello", "", "World!", "", "?")
	fc <- ffchar(cvec)
	expect_identical(fc[], cvec)
	
	is.na(cvec) <- 4
	fc <- ffchar(cvec)
	expect_identical(fc[], cvec)
	
	#cvec[3] <- "Brave New"
	#fc[3] <- "Brave New"
	#expect_identical(fc[], cvec)
})