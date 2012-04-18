\name{unique.ff}
\alias{unique.ff}
\title{Unique values}
\usage{
  \method{unique}{ff} (x, incomparables = FALSE,
    fromLast = FALSE, ...)
}
\arguments{
  \item{x}{\code{ff} object}

  \item{incomparables}{a vector of values that cannot be
  compared.  FALSE is a special value, meaning that all
  values can be compared, and may be the only value
  accepted for methods other than the default.  It will be
  coerced internally to the same type as x.}

  \item{fromLast}{logical indicating if duplication should
  be considered from the last, i.e., the last (or
  rightmost) of identical elements will be kept}

  \item{...}{Further arguments passed to
  \code{\link{unique}}}
}
\value{
  vector with unique values of \code{x}
}
\description{
  Unique values
}

