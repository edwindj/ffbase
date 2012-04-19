\name{ffdfddply}
\alias{ffdfddply}
\title{Performs a split-apply-combine on an ffdf}
\usage{
  ffdfddply(x, split, FUN,
    BATCHBYTES = getOption("ffbatchbytes"),
    RECORDBYTES = sum(.rambytes[vmode(x)]), trace = TRUE,
    ...)
}
\arguments{
  \item{x}{an ffdf}

  \item{split}{an ff vector which is part of the ffdf x}

  \item{FUN}{the function to apply to each split. This
  function needs to return a data.frame}

  \item{BATCHBYTES}{integer scalar limiting the number of
  bytes to be processed in one chunk}

  \item{RECORDBYTES}{optional integer scalar representing
  the bytes needed to process one row of x}

  \item{trace}{logical indicating to show on which split
  the function is computing}

  \item{...}{other parameters passed on to FUN}
}
\value{
  an ffdf
}
\description{
  Performs a split-apply-combine on an ffdf. Splits the x
  ffdf according to split and applies FUN to the data,
  stores the result of the FUN in an ffdf.\cr Remark that
  this function does not actually split the data. In order
  to reduce the number of times data is put into RAM for
  situations with a lot of split levels, the function
  extracts groups of split elements which can be put into
  RAM according to BATCHBYTES. Please make sure your FUN
  covers the fact that several split elements can be in one
  chunk of data on which FUN is applied.
}
\examples{
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
}
\seealso{
  \code{\link{grouprunningcumsum}, \link{table.ff}}
}

