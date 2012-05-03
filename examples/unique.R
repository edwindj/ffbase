data(iris)
irisdouble <- rbind(iris, iris)
ffiris <- as.ffdf(irisdouble)
## unique.ffdf
uiris <- unique(ffiris, trace=TRUE, by=10)[,]
test <- unique(irisdouble)
dim(iris)
dim(irisdouble)
dim(uiris)
dim(test)
!apply(uiris, MARGIN=1, FUN=function(x) paste(x, collapse=",")) %in% apply(test, MARGIN=1, FUN=function(x) paste(x, collapse=","))
!apply(test, MARGIN=1, FUN=function(x) paste(x, collapse=",")) %in% apply(uiris, MARGIN=1, FUN=function(x) paste(x, collapse=",")) 
