\name{ffdfappend}
\alias{ffdfappend}
\title{append a dataframe to a ffdf}
\usage{
  ffdfappend(x, dat, recode = TRUE, ...)
}
\arguments{
  \item{x}{\code{ffdf} object where data will be appended
  to. If \code{x==NULL} a new \code{ff} object will be
  created}

  \item{dat}{\code{ffdf} object or \code{data.frame}
  object}

  \item{recode}{should factors be recoded (default), or not
  (faster)}

  \item{...}{Further arguments passed to
  \code{\link{as.ffdf}}, when \code{x==NULL}}
}
\value{
  \code{ffdf} object with same physical storage as \code{x}
}
\description{
  Appends (\code{ff}) vector to \code{ff} vector \code{x}.
  Please note that the data of \code{y} will be coerced to
  the type of \code{x}.
}
\seealso{
  \code{\link{c.ff}}
}

