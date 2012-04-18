data(iris)
ffiris <- as.ffdf(iris)

result <- ffdfddply(x=ffiris, 
	split=x$Species, 
	FUN=function(x){
		lowestbypetalwidth <- x[order(x$Petal.Width, decreasing=TRUE), ]
		lowestbypetalwidth <- lowestbypetalwidth[!duplicated(lowestbypetalwidth[, c("Species","Petal.Width")]), ]
		lowestbypetalwidth$group <- factor(x= "lowest", levels = c("lowest","highest"))
		highestbypetalwidth <- x[order(x$Petal.Width, decreasing=FALSE), ]
		highestbypetalwidth <- highestbypetalwidth[!duplicated(highestbypetalwidth[, c("Species","Petal.Width")]), ]
		highestbypetalwidth$group <- factor(x= "highest", levels = c("lowest","highest"))
		rbind(lowestbypetalwidth, highestbypetalwidth)
}, 
BATCHBYTES = 5000, 
trace=TRUE)
class(result)
dim(result)
dim(iris)
result[1:10,]
