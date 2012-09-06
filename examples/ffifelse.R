data(iris)
ffiris <- as.ffdf(iris)
ffifelse(ffiris$Sepal.Length < 5, TRUE, NA)
ffifelse(ffiris$Sepal.Length < 5, factor(rep("abc", nrow(ffiris))), NA)
ffifelse(ffiris$Sepal.Length < 5, Sys.Date(), factor("abc"))
ffifelse(ffiris$Sepal.Length < 5, Sys.Date(), ff(seq.Date(Sys.Date()+1, Sys.Date()+nrow(ffiris), by = "day")))


