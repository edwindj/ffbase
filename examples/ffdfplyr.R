data(iris)
ffiris <- as.ffdf(iris)

result <- ffdfdply( x = ffiris
                  , split = ffiris$Species
                  , FUN = function(x){
    dup <- duplicated(x[c("Species", "Petal.Width")])
    o <- order(x$Petal.Width)
    lowest_pw <- x[rev(o),][!dup,]
    highest_pw <- x[o,][!dup,]
    lowest_pw$group <- factor("lowest", levels=c("lowest", "highest"))
    highest_pw$group <- factor("highest", levels=c("lowest", "highest"))
		rbind(lowest_pw, highest_pw)
                   }
                  , BATCHBYTES = 5000
                  , trace=TRUE
                  )
dim(result)
dim(iris)
result[1:10,]
