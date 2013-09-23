\name{table.ff}
\alias{table.ff}
\title{table.ff uses the cross-classifying factors to build a contingency table of
the counts at each combination of factor levels.\cr
If \code{...} does not contain factors, \code{unique.ff} will add a levels
attribute to the non-factors.}
\usage{
  table.ff(..., exclude = if (useNA == "no") c(NA, NaN),
    useNA = c("no", "ifany", "always"),
    dnn = list.names(...), deparse.level = 1)
}
\arguments{
  \item{...}{\code{ff} factors or \code{ff} integers}

  \item{exclude}{see \code{\link{table}}}

  \item{useNA}{see \code{\link{table}}}

  \item{dnn}{see \code{\link{table}}}

  \item{deparse.level}{see \code{\link{table}}}
}
\value{
  \code{table} object
}
\description{
  Details
}
\seealso{
  \code{\link{table}}
}

