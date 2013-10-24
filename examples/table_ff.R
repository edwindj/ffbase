A <- ff(factor(rep(c("A","B","C"), 10)))
a <- ff(factor(rep(c("a","b"), 15)))

table(A,a)

data(iris)
ffiris <- as.ffdf(iris)
ffiris <- transform(ffiris, Sepal.Length = as.integer(Sepal.Length))
ffiris$date <- ff(as.Date(ffiris$Sepal.Length[], origin = "1970-01-01"), vmode = "integer")
table(ffiris$Sepal.Length, ffiris$Species)
table(ffiris$date, ffiris$Species)
