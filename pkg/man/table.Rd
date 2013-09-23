\name{table}
\alias{table}
\title{table.ff uses the cross-classifying factors to build a contingency table of
the counts at each combination of factor levels.\cr
If \code{...} does not contain factors, \code{unique.ff} will add a levels
attribute to the non-factors.}
\usage{
  table(..., exclude = if (useNA == "no") c(NA, NaN),
    useNA = c("no", "ifany", "always"),
    dnn = list.names(...), deparse.level = 1)
}
\description{
  Details
}
\seealso{
  \code{\link{table}} Override table functionality
}

