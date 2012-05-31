iris.ffdf <- as.ffdf(iris)

td <- tempdir()
save.ffdf(iris.ffdf, dir=td)
dir(td)
