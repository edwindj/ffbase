\name{manip_grouped_ffdf}
\alias{arrange.grouped_ffdf}
\alias{filter.grouped_ffdf}
\alias{manip_grouped_ffdf}
\alias{mutate.grouped_ffdf}
\alias{select.grouped_ffdf}
\alias{summarise.grouped_ffdf}
\title{Data manipulation for grouped data tables.}
\usage{
\method{filter}{grouped_ffdf}(.data, ...)

\method{summarise}{grouped_ffdf}(.data, ...)

\method{mutate}{grouped_ffdf}(.data, ..., inplace = FALSE)

\method{arrange}{grouped_ffdf}(.data, ...)

\method{select}{grouped_ffdf}(.data, ...)
}
\arguments{
  \item{.data}{}

  \item{...}{variables interpreted in the context of
  \code{.data}}

  \item{inplace}{if \code{FALSE} (the default) the data
  frame will be copied prior to modification to avoid
  changes propagating via reference.}
}
\description{
Data manipulation for grouped data tables.
}
\examples{
if (require("dplyr")) {
iris_ffdf <- as.ffdf(iris)
species <- group_by(iris_ffdf, Species)
filter(species, Petal.Width = max(Petal.Width))
summarise(players, Petal.Width = mean(Petal.Width))
mutate(species, cSepal.Width = Sepal.Width - min(Sepal.Width))
arrange(species, Species, desc(Sepal.Width))

}
}

