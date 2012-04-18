\name{ffappend}
\alias{ffappend}
\title{Append a ff vector to another ff vector}
\usage{
  ffappend(x, y, ...)
}
\arguments{
  \item{x}{\code{ff} object where data will be appended to.
  If \code{x==NULL} a new \code{ff} object will be created}

  \item{y}{\code{ff} object or vector object}
}
\value{
  \code{ff} object with same physical storage as \code{x}
}
\description{
  Appends (\code{ff}) vector \code{y} to \code{ff} vector
  \code{x}. Please note that the data of \code{y} will be
  coerced to the type of \code{x}.
}
\seealso{
  \code{\link{c.ff}}
}

