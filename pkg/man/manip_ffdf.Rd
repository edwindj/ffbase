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
filter.ffdf(.data, ..., env = parent.frame())

filter.tbl_ffdf(.data, ..., env = parent.frame())

summarise.ffdf(.data, ...)

summarise.tbl_ffdf(.data, ...)

mutate.ffdf(.data, ..., inplace = FALSE)

mutate.tbl_ffdf(.data, ...)

arrange.ffdf(.data, ...)

arrange.tbl_ffdf(.data, ...)

select.ffdf(.data, ...)

select.tbl_ffdf(.data, ...)

do.ffdf(.data, .f, ...)

do.tbl_ffdf(.data, .f, ...)
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
if (require(dplyr)){
   mtcars <- as.ffdf(mtcars)
   filter(mtcars, cyl == 8)
   select(mtcars, mpg, cyl, hp:vs)
   arrange(mtcars, cyl, disp)
   mutate(mtcars, displ_l = disp / 61.0237)
   summarise(mtcars, mean(disp))
   #summarise(group_by(mtcars, cyl), mean(disp))
}
}

