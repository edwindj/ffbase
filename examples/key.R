oldffmaxbytes <- getOption("ffmaxbytes")
options(ffmaxbytes = 20)
ffiris <- as.ffdf(iris)
ffiris$key1 <- key(ffiris)
ffiris$key2 <- key(ffiris[c("Petal.Width","Species")])
unique(ffiris[c("key2","Petal.Width","Species")])[,]
options(ffmaxbytes = oldffmaxbytes)

