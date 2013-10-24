library(testthat)

context("with")

test_that("Chunkify a function",{
	dat <- data.frame(x=1:10, y=10:1) 
	ffdat <- as.ffdf(dat)
   
  z <- with(dat, x+y)
  fz <- with(ffdat, x+y)
     
	expect_equivalent( z
	                , fz[]
				    )
})

test_that("Creating a character vector works", {
  dat <- data.frame(x=1:10, y=10:1) 
  ffdat <- as.ffdf(dat)
  
  z <- with(dat, paste(x,y,sep=":"))
  fz <- ffdfwith(ffdat, paste(x,y,sep=":"), by=5)
  
  expect_equivalent( factor(z)
                   , fz[]
                   )  
})

test_that("Creating a factor vector works", {
  dat <- data.frame(x=1:10, y=10:1)
  ffdat <- as.ffdf(dat)
  
  z <- with(dat, factor(paste(x,y, sep=":")))
  fz <- ffdfwith(ffdat, factor(paste(x,y, sep=":")), by=5)
  
  expect_equivalent( z
                   , fz[]
                   )  
})

test_that("Creating a factor data.frame works", {
  dat <- data.frame(x=1:10, y=10:1)
  ffdat <- as.ffdf(dat)
  
  z <- with(dat, data.frame(a=factor(paste(x,y, sep=":"))))
  fz <- ffdfwith(ffdat, data.frame(a=factor(paste(x,y, sep=":"))), by=5)
  
  expect_equivalent( z
                   , fz[,,drop=FALSE]
                   )  
})