library(testthat)

context("append")

test_that("Appending ff vector works",{
   x <- c(1,3)
   y <- c(2,3)
   fx <- ff(x)
   fy <- ff(y)
   
   fz <- ffappend(NULL, x)
   expect_equal(length(fz), length(x))
   fz <- ffappend(fz, y)
   expect_equal(length(fz), length(x)+length(y))
   expect_identical(fz[], c(x,y))
   
   fz <- ffappend(NULL, fx)
   expect_equal(length(fz), length(fx))
   fz <- ffappend(fz, fy)
   expect_equal(length(fz), length(fx)+length(fy))
   expect_identical(fz[], c(x,y))
})

test_that("Concatenating ff vector works",{
   x <- ff(c(1,3))
   y <- ff(c(2,3))
   z <- c(x)
   expect_equal(length(z), length(x))
   z <- c(x, y)
   expect_equal(length(z), length(x)+length(y))
   expect_identical(z[], c(x[],y[]))
   z <- c(x, y, x)
   expect_equal(length(z), length(x)+length(y)+length(x))
})

test_that("Appending dataframe to ffdf works",{
   dat <- data.frame(x=1:3, y=3:1, z=as.factor(c("a","b","c")))
	fdat <- NULL
	fdat <-ffdfappend(fdat, dat)
	expect_identical(dat,fdat[,])
	fdat <- ffdfappend(fdat,dat)
	expect_equal(nrow(fdat),2*nrow(dat))
})