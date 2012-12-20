oldffmaxbytes <- getOption("ffmaxbytes")
options(ffmaxbytes = 20)
ffiris <- as.ffdf(iris)
ffiris$key1 <- ikey(ffiris)
ffiris$key2 <- ikey(ffiris[c("Petal.Width","Species")])
unique(ffiris[c("key2","Petal.Width","Species")])[,]
options(ffmaxbytes = oldffmaxbytes)

