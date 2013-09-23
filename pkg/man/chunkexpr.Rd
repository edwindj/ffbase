\name{chunkexpr}
\alias{chunkexpr}
\title{Chunk an expression}
\usage{
  chunkexpr(expr, x = all.vars(expr), i = ".i",
    prefix = "")
}
\arguments{
  \item{expr}{\code{expression} vector or language object}

  \item{x}{\code{character} with variables to be chunked}

  \item{i}{name of index that will be used in the for loop,
  typically a \code{ri} or \code{hi}.}

  \item{prefix}{prefix for variables to be replaced.}
}
\description{
  chunkexpr replaces variables in an expression with a
  indexed version. It main use it to rewrite "normal" R
  expression into chunked versions that can be evaluated in
  a chunked-for-loop.
}
\examples{
expr <- expression( x <- 1)
ffbase:::chunkexpr(expr)
# results in: expression(x[.i] <- 1)

ffbase:::chunkexpr(expression(z <- x+y), c("y","z"), prefix="data$")
# results in: expression(data$z[.i] <- x + data$y[.i])
}
\keyword{internal}

