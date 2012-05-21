library(testthat)
library(ff)

context("operators")

test_that("abs works",{
  x <- rnorm(1000000)
  test.ram <- abs(x)
  test.ff <- abs(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("sign works",{
  x <- rnorm(1000000)
  test.ram <- sign(x)
  test.ff <- sign(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("sqrt works",{
  x <- rnorm(1000000)
  test.ram <- sqrt(x)
  test.ff <- sqrt(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("ceiling works",{
  x <- rnorm(1000000)
  test.ram <- ceiling(x)
  test.ff <- ceiling(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("floor works",{
  x <- rnorm(1000000)
  test.ram <- floor(x)
  test.ff <- floor(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("trunc works",{
  x <- rnorm(1000000)
  test.ram <- trunc(x)
  test.ff <- trunc(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("log10 works",{
  x <- rnorm(1000000)
  test.ram <- log10(x)
  test.ff <- log10(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("log2 works",{
  x <- rnorm(1000000)
  test.ram <- log2(x)
  test.ff <- log2(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("log1p works",{
  x <- rnorm(1000000)
  test.ram <- log1p(x)
  test.ff <- log1p(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("acos works",{
  x <- rnorm(1000000)
  test.ram <- acos(x)
  test.ff <- acos(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("acosh works",{
  x <- rnorm(1000000)
  test.ram <- acosh(x)
  test.ff <- acosh(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("asin works",{
  x <- rnorm(1000000)
  test.ram <- asin(x)
  test.ff <- asin(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("asinh works",{
  x <- rnorm(1000000)
  test.ram <- asinh(x)
  test.ff <- asinh(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("atan works",{
  x <- rnorm(1000000)
  test.ram <- atan(x)
  test.ff <- atan(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("atanh works",{
  x <- rnorm(1000000)
  test.ram <- atanh(x)
  test.ff <- atanh(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("exp works",{
  x <- rnorm(1000000)
  test.ram <- exp(x)
  test.ff <- exp(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("expm1 works",{
  x <- rnorm(1000000)
  test.ram <- expm1(x)
  test.ff <- expm1(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("cos works",{
  x <- rnorm(1000000)
  test.ram <- cos(x)
  test.ff <- cos(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("cosh works",{
  x <- rnorm(1000000)
  test.ram <- cosh(x)
  test.ff <- cosh(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("sin works",{
  x <- rnorm(1000000)
  test.ram <- sin(x)
  test.ff <- sin(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("sinh works",{
  x <- rnorm(1000000)
  test.ram <- sinh(x)
  test.ff <- sinh(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("tan works",{
  x <- rnorm(1000000)
  test.ram <- tan(x)
  test.ff <- tan(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("tanh works",{
  x <- rnorm(1000000)
  test.ram <- tanh(x)
  test.ff <- tanh(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("gamma works",{
  x <- rnorm(1000000)
  test.ram <- gamma(x)
  test.ff <- gamma(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("lgamma works",{
  x <- rnorm(1000000)
  test.ram <- lgamma(x)
  test.ff <- lgamma(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("digamma works",{
  x <- rnorm(1000000)
  test.ram <- digamma(x)
  test.ff <- digamma(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("trigamma works",{
  x <- rnorm(1000000)
  test.ram <- trigamma(x)
  test.ff <- trigamma(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("log works",{
  x <- rnorm(1000000)
  test.ram <- log(x)
  test.ff <- log(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("round works",{
  x <- rnorm(1000000)
  test.ram <- round(x)
  test.ff <- round(ff(x))  
  expect_equal(test.ram, test.ff[])
})
test_that("signif works",{
  x <- rnorm(1000000)
  test.ram <- signif(x)
  test.ff <- signif(ff(x))  
  expect_equal(test.ram, test.ff[])
})

test_that("operator == works",{
  size <- 1000000
  x <- rnorm(size)
  y <- rnorm(size)
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx]
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx] <- NA
  test.ram <- x == y
  test.ff <- as.ff(x) == as.ff(y)
  expect_equal(test.ram, test.ff[])
})
test_that("operator < works",{
  size <- 1000000
  x <- rnorm(size)
  y <- rnorm(size)
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx]
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx] <- NA
  test.ram <- x < y
  test.ff <- as.ff(x) < as.ff(y)
  expect_equal(test.ram, test.ff[])
})
test_that("operator <= works",{
  size <- 1000000
  x <- rnorm(size)
  y <- rnorm(size)
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx]
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx] <- NA
  test.ram <- x <= y
  test.ff <- as.ff(x) <= as.ff(y)
  expect_equal(test.ram, test.ff[])
})
test_that("operator > works",{
  size <- 1000000
  x <- rnorm(size)
  y <- rnorm(size)
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx]
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx] <- NA
  test.ram <- x > y
  test.ff <- as.ff(x) > as.ff(y)
  expect_equal(test.ram, test.ff[])
})
test_that("operator >= works",{
  size <- 1000000
  x <- rnorm(size)
  y <- rnorm(size)
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx]
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx] <- NA
  test.ram <- x >= y
  test.ff <- as.ff(x) >= as.ff(y)
  expect_equal(test.ram, test.ff[])
})
test_that("operator + works",{
  size <- 1000000
  x <- rnorm(size)
  y <- rnorm(size)
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx]
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx] <- NA
  test.ram <- x + y
  test.ff <- as.ff(x) + as.ff(y)
  expect_equal(test.ram, test.ff[])
})
test_that("operator - works",{
  size <- 1000000
  x <- rnorm(size)
  y <- rnorm(size)
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx]
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx] <- NA
  test.ram <- x - y
  test.ff <- as.ff(x) - as.ff(y)
  expect_equal(test.ram, test.ff[])
})
test_that("operator * works",{
  size <- 1000000
  x <- rnorm(size)
  y <- rnorm(size)
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx]
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx] <- NA
  test.ram <- x * y
  test.ff <- as.ff(x) * as.ff(y)
  expect_equal(test.ram, test.ff[])
})
test_that("operator / works",{
  size <- 1000000
  x <- rnorm(size)
  y <- rnorm(size)
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx]
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx] <- NA
  test.ram <- x / y
  test.ff <- as.ff(x) / as.ff(y)
  expect_equal(test.ram, test.ff[])
})
test_that("operator ^ works",{
  size <- 1000000
  x <- rnorm(size)
  y <- rnorm(size)
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx]
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx] <- NA
  test.ram <- x ^ y
  test.ff <- as.ff(x) ^ as.ff(y)
  expect_equal(test.ram, test.ff[])
})
test_that("operator %% works",{
  size <- 1000000
  x <- rnorm(size)
  y <- rnorm(size)
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx]
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx] <- NA
  test.ram <- x %% y
  test.ff <- as.ff(x) %% as.ff(y)
  expect_equal(test.ram, test.ff[])
})
test_that("operator %/% works",{
  size <- 1000000
  x <- rnorm(size)
  y <- rnorm(size)
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx]
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx] <- NA
  test.ram <- x %/% y
  test.ff <- as.ff(x) %/% as.ff(y)
  expect_equal(test.ram, test.ff[])
})

test_that("operator & works",{
  size <- 1000000
  x <- rnorm(size) > 0
  y <- rnorm(size) > 0
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx]
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx] <- NA
  test.ram <- x & y
  test.ff <- as.ff(x) & as.ff(y)
  expect_equal(test.ram, test.ff[])
})
test_that("operator | works",{
  size <- 1000000
  x <- rnorm(size) > 0
  y <- rnorm(size) > 0
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx]
  idx <- sample(10000, 1:size)
  x[idx] <- y[idx] <- NA
  test.ram <- x | y
  test.ff <- as.ff(x) | as.ff(y)
  expect_equal(test.ram, test.ff[])
})
test_that("operator ! works",{
  size <- 1000000
  x <- rnorm(size) > 0
  idx <- sample(10000, 1:size)
  x[idx] <- NA
  test.ram <- !x
  test.ff <- !as.ff(x)
  expect_equal(test.ram, test.ff[])
})

