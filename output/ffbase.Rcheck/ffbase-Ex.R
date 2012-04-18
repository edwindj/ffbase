pkgname <- "ffbase"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ffbase')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("ffbase-package")
### * ffbase-package

flush(stderr()); flush(stdout())

### Name: ffbase-package
### Title: Basic statistical functions for ff
### Aliases: ffbase ffbase-package

### ** Examples


ffdat <- as.ffdf(data.frame(x=1:10, y=10:1))

# add a new ff vector z to the ffdf data.frame
within(ffdat, {z <- x+y})[]

# add a new ff vector z to the ffdf data.frame using transform
transform(ffdat, z=x+y)[]

cut(ffdat$x, breaks=3)[]

tabulate.ff(ffdat$x)



cleanEx()
nameEx("ffdfddply")
### * ffdfddply

flush(stderr()); flush(stdout())

### Name: ffdfddply
### Title: Performs a split-apply-combine on an ffdf
### Aliases: ffdfddply

### ** Examples

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



cleanEx()
nameEx("ffdfsave")
### * ffdfsave

flush(stderr()); flush(stdout())

### Name: ffdfsave
### Title: Save a ffdf data.frame in directory
### Aliases: ffdfsave

### ** Examples


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



cleanEx()
nameEx("ffwhich")
### * ffwhich

flush(stderr()); flush(stdout())

### Name: ffwhich
### Title: Create an index from a filter statement 'ffwhich' creates an
###   'ff' integer index vector from a filter expression. The resulting
###   vector can be used to index or subset a ffdf or ff vector.
### Aliases: ffwhich

### ** Examples

# create a ff vector
x <- ff(10:1)
# make an ff index vector
idx <- ffwhich(x, x < 5)
# use it to retrieve values from x
x[idx][]

# create a ffdf data.frame
dat <- ffdf(x1=x, y1=x)
# create an ff index vector from a filter statement
idx <- ffwhich(dat, x1 < 5 & y1 > 2)
# use it to select data from the data.frame
dat[idx,][,]



cleanEx()
nameEx("mean.ff")
### * mean.ff

flush(stderr()); flush(stdout())

### Name: mean.ff
### Title: Mean of ff vector
### Aliases: mean.ff

### ** Examples

# create a vector of length 10 million
x <- ff(vmode="double", length=1e7)

mean(x)



cleanEx()
nameEx("min.ff")
### * min.ff

flush(stderr()); flush(stdout())

### Name: min.ff
### Title: Minimum, maximum and range of ff vector
### Aliases: max max.ff min min.ff range range.ff

### ** Examples

x <- ff(1:100)

min(x)
max(x)
range(x)

is.na(x) <- 10
min(x)
max(x)
range(x)


min(x, na.rm=TRUE)
max(x, na.rm=TRUE)
range(x, na.rm=TRUE)



cleanEx()
nameEx("tabulate.ff")
### * tabulate.ff

flush(stderr()); flush(stdout())

### Name: tabulate.ff
### Title: Tabulation for ff vectors
### Aliases: tabulate.ff

### ** Examples


#create a vector of 10 million
x <- ff(vmode="integer", length=1e7)

# fill first 200 with values
x[1:100] <- 1
x[101:200] <- 2

# lets count
tabulate.ff(x)



cleanEx()
nameEx("transform.ffdf")
### * transform.ffdf

flush(stderr()); flush(stdout())

### Name: transform.ffdf
### Title: Transform a ffdf data.frame
### Aliases: transform.ffdf

### ** Examples


transform(as.ffdf(airquality), Ozone = -Ozone)
transform(as.ffdf(airquality), new = -Ozone, Temp = (Temp-32)/1.8)



cleanEx()
nameEx("with.ffdf")
### * with.ffdf

flush(stderr()); flush(stdout())

### Name: with.ffdf
### Title: Evaluate an expression in a ffdf data environment
### Aliases: with.ffdf

### ** Examples

dat <- data.frame(x=1:10, y=10:1)

ffdat <- as.ffdf(dat)

with(dat, {x+y})



cleanEx()
nameEx("within.ffdf")
### * within.ffdf

flush(stderr()); flush(stdout())

### Name: within.ffdf
### Title: Evaluate an expression in a ffdf data environment
### Aliases: within.ffdf

### ** Examples


ffdat <- as.ffdf(data.frame(x=1:10, y=10:1))
# add z to the ffdat
within(ffdat, {z <- x+y})



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
