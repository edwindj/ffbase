\name{bySum}
\alias{bySum}
\title{Fast conditional sum}
\usage{
bySum(x, by, na.rm = FALSE, weight = NULL, ...)
}
\arguments{
  \item{x}{\code{numeric} vector to be summed}

  \item{by}{(list of) \code{factor(s)} for which the sum
  will be calculated}

  \item{na.rm}{\code{logical} If \code{TRUE} \code{NA}
  values will be removed}

  \item{weight}{\code{numeric} with of same length as
  \code{x}}

  \item{...}{not used}
}
\value{
\code{array} with dimensions of \code{by}
}
\description{
\code{bySum} works like a very fast version of tapply with
(weighted) \code{FUN=sum}.
}

