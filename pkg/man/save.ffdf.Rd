\name{save.ffdf}
\alias{save.ffdf}
\title{Save ffdf data.frames in a directory}
\usage{
  save.ffdf(..., dir = "./ffdb", clone = FALSE,
    relativepath = TRUE)
}
\arguments{
  \item{...}{\code{ffdf} data.frames, \code{ff} vectors, or
  other variables to be saved in the directory}

  \item{dir}{path where .rdata file will be saved and all
  columns of supplied \code{ffdf}'s. It will be created if
  it doesn't exist.}

  \item{clone}{should the data.frame be cloned?}

  \item{relativepath}{\code{logical} if \code{TRUE} the
  stored ff vectors will have relative paths, making moving
  the data to another storage a simple copy operation.}
}
\description{
  \code{save.ffdf} saves all ffdf data.frames in the given
  \code{dir}. Each column is stored as with filename
  <ffdfname>$<colname>.ff. All variables given in "..." are
  stored in ".RData" in the same directory. The data can be
  reloaded by starting a R session in the directory or by
  using \code{\link{load.ffdf}}.
}
\examples{
iris.ffdf <- as.ffdf(iris)

td <- tempfile()

# save the ffdf into the supplied directory
save.ffdf(iris.ffdf, dir=td)

# what in the directory?
dir(td)

#remove the ffdf from memory
rm("iris.ffdf")

# and reload the stored ffdf
load.ffdf(dir=td)

tf <- paste(tempfile(), ".zip", sep="")
packed <- pack.ffdf(file=tf, iris.ffdf)

#remove the ffdf from memory
rm("iris.ffdf")

# restore the ffdf from the packed ffdf
unpack.ffdf(tf)
}
\seealso{
  \code{\link{load.ffdf}}
}

