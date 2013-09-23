\name{ffdfwith}
\alias{ffdfwith}
\title{Evaluate an expression in a ffdf data environment}
\usage{
  ffdfwith(data, expr, ...)
}
\arguments{
  \item{data}{\code{\link{ffdf}} data object used as an
  environment for evaluation.}

  \item{expr}{expression to evaluate.}

  \item{...}{arguments to be passed to future methods.}
}
\value{
  if expression is a \code{vector} a newly created
  \code{ff} vector will be returned otherwise if the
  expression is a data.frame a newly created \code{ffdf}
  object will be returned.
}
\description{
  Evaluate an R expression in an environment constructed
  from a ffdata data frame. Faster than
  \code{\link{with.ffdf}}, but in constrast \code{ffdfwith}
  can change the original data. Please note that you should
  write your expression as if it is a normal
  \code{data.frame}. The resulting return value however
  will be a \code{ffdf} object.
}
\examples{
dat <- data.frame(x=1:10, y=10:1)
ffdat <- as.ffdf(dat)

ffdfwith(ffdat, {
   x <- x + 1
   x + y
})

#notice that x has been altered
ffdat$x
}

