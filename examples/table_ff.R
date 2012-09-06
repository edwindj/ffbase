A <- ff(factor(rep(c("A","B","C"), 10)))
a <- ff(factor(rep(c("a","b"), 15)))

table.ff(A,a)


data(iris)
ffiris <- as.ffdf(iris)
ffiris$Sepal.Length <- with(ffiris, as.integer(Sepal.Length))
ffiris$date <- ff(as.Date(ffiris$Sepal.Length[], origin = "1970-01-01"), vmode = "integer")
table.ff(ffiris$Sepal.Length, ffiris$Species)
table.ff(ffiris$date, ffiris$Species)


