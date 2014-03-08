# library(ffbase)
# data(iris)
# a <- as.ffdf(iris)
# save.ffdf(a, dir="c:/test/ff2/", overwrite=T)
# idx <- ff(1L)
# a <- a[idx,]
# #gc()
# save.ffdf(a, dir="c:/test/ff2/", overwrite=T)
# unlink("c:/test/ff", recursive=T)
# 
# a <- as.ffdf(iris)
# b <- a # b points to same files as a
# save.ffdf(a, dir="c:/test/ff", overwrite=T)
# open(b)
# a <- a[ff(1L),]
# save.ffdf(a, dir="c:/test.ff", overwrite=T)
library(ffbase)
data(iris)
a <- as.ffdf(iris)
save.ffdf(a, dir="c:/test/ff", overwrite=T)
rm(a)
load.ffdf("c:/test/ff")
idx <- ffdforder(a[c("Sepal.Length","Sepal.Width")])
b <- a
a <- a[idx,] # this creates a new ffdf frame in the temp dir, but the original files are still open 
close(b)
gc() # garbage collect memory and close old a files.
save.ffdf(a, dir="c:/test/ff", overwrite=T) # this doesn't work :-(
