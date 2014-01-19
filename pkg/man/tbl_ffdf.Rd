\name{tbl_ffdf}
\alias{tbl_ffdf}
\title{Create a ffdf source.}
\usage{
tbl_ffdf(data)
}
\arguments{
  \item{data}{a ffdf data.frame}
}
\description{
A ffdf source wraps a local ffdf.
}
\examples{
if (require("ffbase")) {
ds <- tbl_ffdf(mtcars)
ds
}
}

