\name{manip_ffdf}
\alias{arrange.ffdf}
\alias{arrange.tbl_ffdf}
\alias{do.ffdf}
\alias{do.tbl_ffdf}
\alias{filter.ffdf}
\alias{filter.tbl_ffdf}
\alias{manip_ffdf}
\alias{mutate.ffdf}
\alias{mutate.tbl_ffdf}
\alias{select.ffdf}
\alias{select.tbl_ffdf}
\alias{summarise.ffdf}
\alias{summarise.tbl_ffdf}
\title{Data manipulation for ffdf.}
\usage{
\method{filter}{ffdf}(.data, ..., env = parent.frame())

\method{filter}{tbl_ffdf}(.data, ..., env = parent.frame())

\method{summarise}{ffdf}(.data, ...)

\method{summarise}{tbl_ffdf}(.data, ...)

\method{mutate}{ffdf}(.data, ..., inplace = FALSE)

\method{mutate}{tbl_ffdf}(.data, ...)

\method{arrange}{ffdf}(.data, ...)

\method{arrange}{tbl_ffdf}(.data, ...)

\method{select}{ffdf}(.data, ...)

\method{select}{tbl_ffdf}(.data, ...)

\method{do}{ffdf}(.data, .f, ...)

\method{do}{tbl_ffdf}(.data, .f, ...)
}
\arguments{
  \item{.data}{a ffdf}

  \item{...}{variables interpreted in the context of
  \code{.data}}

  \item{inplace}{if \code{FALSE} (the default) the data
  frame will be copied prior to modification to avoid
  changes propagating via reference.}
}
\description{
Data manipulation for ffdf.
}
\examples{
if (require("ggplot2")) {
# If you start with a ffdf, you end up with a ffdf
diamonds <- as.ffdf(diamonds)
filter(diamonds, color == 'E', cut == 'Good')
}
}

