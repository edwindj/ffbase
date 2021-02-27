test_that("binned_sum works", {
  iris_ff <- as.ffdf(iris)
  res <- binned_sum(iris_ff$Sepal.Length, iris_ff$Species)
  res
  res2 <- binned_sum(iris$Sepal.Length, iris$Species)
  expect_equal(res, res2)
})


test_that("binned_sumsq works", {
  iris_ff <- as.ffdf(iris)
  res <- binned_sumsq(iris_ff$Sepal.Length, bin=iris_ff$Species)
  res
  res2 <- binned_sumsq(iris$Sepal.Length, bin=iris$Species)
  expect_equal(res, res2)
})

test_that("binned_sum works with INDEX", {
  iris_ff <- as.ffdf(iris)
  res <- binned_sum(iris_ff$Sepal.Length, iris_ff$Species, INDEX=NULL)
  res
  res2 <- binned_sum(iris$Sepal.Length, iris$Species)
  expect_equal(res, res2)
})
