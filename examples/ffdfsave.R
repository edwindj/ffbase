
data(iris)

# create a ffdf data.frame from standard iris data set
ffiris <- as.ffdf(iris)
head(ffiris[,])

.fn <- tempfile()
ffdfsave(ffiris, .fn)

# clear everything
rm(list=ls())
ls()

# load ffdf into environment
load(file=.fn)
# and back in business!
head(ffiris[,])
