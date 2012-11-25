library(testthat)
library(ff)

context("subset")

test_that("Subsetting ff vector works",{
   x <- 1:10
   ss <- x < 5
   fx <- ff(x)
   fss <- ff(ss)
   expect_identical(subset(x, ss), subset(fx,fss)[])   
})

test_that("Subsetting ffdf works",{
  x <- iris
  fx <- as.ffdf(iris)
  
  sx <- subset(x)
  sfx <- subset(fx)
  
  expect_equivalent(sfx[,], sx)
  
  sx <- subset(x, select=Species)
  sfx <- subset(fx, select=Species)
  expect_equivalent(sfx[,,drop=FALSE], sx)
  
  sx <- subset(x, subset=Species=="setosa", select=Species)
  sfx <- subset(fx, subset=Species=="setosa", select=Species)
  expect_equivalent(sfx[,,drop=FALSE], sx)
  
})