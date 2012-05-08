data(iris)
irisdouble <- rbind(iris, iris)
ffiris <- as.ffdf(irisdouble)
## unique.ff
unique(ffiris$Sepal.Length)
unique(ffiris$Petal.Length)  
ffiris$Species[1] <- NA
unique(ffiris$Species)
levels(unique(ffiris$Species))
## unique.ffdf
uiris <- unique(ffiris, trace=TRUE, by=10)[,]
test <- unique(irisdouble)
dim(iris)
dim(irisdouble)
dim(uiris)
dim(test)
!apply(uiris, MARGIN=1, FUN=function(x) paste(x, collapse=",")) %in% apply(test, MARGIN=1, FUN=function(x) paste(x, collapse=","))
!apply(test, MARGIN=1, FUN=function(x) paste(x, collapse=",")) %in% apply(uiris, MARGIN=1, FUN=function(x) paste(x, collapse=",")) 



