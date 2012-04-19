\docType{package}
\name{ffbase-package}
\alias{ffbase}
\alias{ffbase-package}
\title{Basic statistical functions for ff}
\description{
  Basic statistical functions for \code{\link{ff}} vectors
  and \code{\link{ffdf}} data.frames. The aim of ffbase is
  to make working with ff vectors and ffdf data.frame a
  little bit easier.
}
\details{
  \itemize{ \item{\code{\link{cut.ff}}}
  \item{\code{\link{tabulate.ff}}}
  \item{\code{\link{table.ff}}}
  \item{\code{\link{unique.ff}}}
  \item{\code{\link{transform.ffdf}}}
  \item{\code{\link{with.ffdf}}}
  \item{\code{\link{within.ffdf}}} }
}
\examples{

ffdat <- as.ffdf(data.frame(x=1:10, y=10:1))

# add a new ff vector z to the ffdf data.frame
within(ffdat, {z <- x+y})[]

# add a new ff vector z to the ffdf data.frame using transform
transform(ffdat, z=x+y)[]

cut(ffdat$x, breaks=3)[]

tabulate.ff(ffdat$x)
}

