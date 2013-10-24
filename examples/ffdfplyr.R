data(iris)
ffiris <- as.ffdf(iris)

youraggregatorFUN <- function(x){
	dup <- duplicated(x[c("Species", "Petal.Width")])
  o <- order(x$Petal.Width)
  lowest_pw <- x[rev(o),][!dup,]
  highest_pw <- x[o,][!dup,]
  lowest_pw$group <- factor("lowest", levels=c("lowest", "highest"))
  highest_pw$group <- factor("highest", levels=c("lowest", "highest"))
	rbind(lowest_pw, highest_pw)
}
result <- ffdfdply( x = ffiris, split = ffiris$Species,
                   FUN = function(x) youraggregatorFUN(x),
                   BATCHBYTES = 5000, trace=TRUE)
dim(result)
dim(iris)
result[1:10,]

ffiris$integerkey <- ffdfwith(ffiris, as.integer(Sepal.Length))
result <- ffdfdply( x = ffiris, split = ffiris$integerkey
                  , FUN = function(x) youraggregatorFUN(x), BATCHBYTES = 5000
                  , trace=TRUE
                  )

ffiris$datekey <- ff( as.Date(ffiris$Sepal.Length[], origin = "1970-01-01"),
                      vmode = "integer")
result <- ffdfdply( x = ffiris, split = ffiris$datekey 
                  , FUN = function(x) youraggregatorFUN(x)
                  , BATCHBYTES = 5000, trace=TRUE
                  )
