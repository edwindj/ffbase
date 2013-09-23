\name{cumsum.ff}
\alias{cummax.ff}
\alias{cummin.ff}
\alias{cumprod.ff}
\alias{cumsum.ff}
\title{Cumulative Sums, Products, and Extremes}
\usage{
  \method{cumsum}{ff} (x, ...)

  \method{cumprod}{ff} (x, ...)

  \method{cummax}{ff} (x, ...)

  \method{cummin}{ff} (x, ...)
}
\arguments{
  \item{x}{\code{ff} numeric vector or an object that can
  be coerced to one a numeric vector}

  \item{...}{other parameters passed on to chunk}
}
\value{
  An \code{ff} vector of the same length and type as x
  (after coercion), except that cumprod returns a numeric
  vector for integer input.\cr An NA value in x causes the
  corresponding and following elements of the return value
  to be NA, as does integer overflow in cumsum (with a
  warning).
}
\description{
  Cumulative Sums, Products, and Extremes
}
\examples{
x <- 1:10000
tmp <- cumsum.ff(ff(x), BATCHBYTES=10000)
class(tmp)
table(tmp[] == cumsum(x))

x <- rnorm(1000)
tmp <- cummax.ff(ff(x), BATCHBYTES=10000)
table(tmp[] == cummax(x))
tmp <- cummin.ff(ff(x), BATCHBYTES=10000)
table(tmp[] == cummin(x))
tmp <- cumprod.ff(ff(x), BATCHBYTES=10000)
table(tmp[] == cumprod(x))

## S3 type of calling
cumsum(ff(x))
cummax(ff(x))
cummin(ff(x))
cumprod(ff(x))


}
\seealso{
  \code{\link{cumsum}}, \code{\link{cumprod}},
  \code{\link{cummax}}, \code{\link{cummin}}
}

