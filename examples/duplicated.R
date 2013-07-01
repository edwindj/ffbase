## duplicated.ffdf - mark that you need to order according to the records you 
## like in order to have similar results as the base unique method 
data(iris)
irisdouble <- rbind(iris, iris)
irisdouble <- irisdouble[ sample(x=1:nrow(irisdouble), size=nrow(irisdouble)
                        , replace = FALSE), ]
ffiris <- as.ffdf(irisdouble)
duplicated(ffiris, by=10, trace=TRUE)
duplicated(ffiris$Sepal.Length, by=10, trace=TRUE)
table(duplicated(irisdouble), duplicated(ffiris, by=10)[])
irisdouble <- irisdouble[order(apply( irisdouble
                                    , FUN=function(x) paste(x, collapse=".")
                                    , MARGIN=1
                                    )), ]
ffiris <- as.ffdf(irisdouble)
table(duplicated(irisdouble), duplicated(ffiris, by=10)[])
table(duplicated(ffiris$Sepal.Width, by=10)[], duplicated(ffiris$Sepal.Width[]))

measures <- c("Sepal.Width","Species")
irisdouble <- irisdouble[order(apply( irisdouble[, measures]
                                    , FUN=function(x) paste(x, collapse=".")
                                    , MARGIN=1)), ]
ffiris <- as.ffdf(irisdouble)
table(duplicated(irisdouble[, measures]), duplicated(ffiris[measures], by=10)[])
table(duplicated(ffiris$Sepal.Width, by=10)[], duplicated(ffiris$Sepal.Width[]))
