\name{merge.ffdf}
\alias{merge.ffdf}
\title{Merge two ffdf by common columns, or do other versions of database join operations.}
\usage{
  \method{merge}{ffdf} (x, y,
    by = intersect(names(x), names(y)), by.x = by,
    by.y = by, all = FALSE, all.x = all, all.y = all,
    sort = FALSE, suffixes = c(".x", ".y"),
    incomparables = NULL, trace = FALSE, ...)
}
\arguments{
  \item{x}{an ffdf}

  \item{y}{an ffdf}

  \item{by}{specifications of the common columns. Columns
  can be specified by name, number or by a logical vector.}

  \item{by.x}{specifications of the common columns of the x
  ffdf, overruling the by parameter}

  \item{by.y}{specifications of the common columns of the y
  ffdf, overruling the by parameter}

  \item{all}{see \code{\link{merge}} in R base}

  \item{all.x}{if TRUE, then extra rows will be added to
  the output, one for each row in x that has no matching
  row in y. These rows will have NAs in those columns that
  are usually filled with values from y. The default is
  FALSE, so that only rows with data from both x and y are
  included in the output.}

  \item{all.y}{similar as all.x}

  \item{sort}{logical, currently not used yet, defaults to
  FALSE.}

  \item{suffixes}{character(2) specifying the suffixes to
  be used for making non-by names() unique.}

  \item{incomparables}{values which cannot be matched. See
  \code{\link{match}}. Currently not used.}

  \item{trace}{logical indicating to show on which chunk
  the function is computing}

  \item{...}{other options passed on to
  \code{\link[ff]{ffdfindexget}}}
}
\value{
  an ffdf
}
\description{
  Merge two ffdf by common columns, or do other versions of
  database join operations. This method is similar to
  \code{\link[base]{merge}} in the \code{base} package but
  only allows inner and left outer joins. Note that joining
  is done based on \code{\link{ffmatch}} or
  \code{\link{ffdfmatch}}: only the first element in
  \code{y} will be added to \code{x}; and since
  \code{\link{ffdfmatch}} works by
  \code{\link[base]{paste}}-ing together a key, this might
  not be suited if your key contains columns of vmode
  double.
}
\details{
  If a left outer join is performed and no matching record
  in x is found in y, columns with vmodes 'boolean',
  'quad', 'nibble', 'ubyte', 'ushort' are coerced
  respectively to vmode 'logical', 'byte', 'byte', 'short',
  'integer' to allow NA values.
}
\examples{
authors <- data.frame(
    surname = c("Tukey", "Venables", "Tierney", "Ripley", "McNeil"),
    nationality = c("US", "Australia", "US", "UK", "Australia"),
    deceased = c("yes", rep("no", 4)))
books <- data.frame(
    name = c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core"),
    title = c("Exploratory Data Analysis",
              "Modern Applied Statistics ...",
              "LISP-STAT",
              "Spatial Statistics", "Stochastic Simulation",
              "Interactive Data Analysis",
              "An Introduction to R"),
    other.author = c(NA, "Ripley", NA, NA, NA, NA,
                     "Venables & Smith"))
books <- lapply(1:2000, FUN=function(x, books){
	books$price <- rnorm(nrow(books))
	books
}, books=books)
books <- do.call(rbind, books)
authors <- as.ffdf(authors)                
books <- as.ffdf(books)

dim(books)
dim(authors)
## Inner join
oldffbatchbytes <- getOption("ffbatchbytes")
options(ffbatchbytes = 100)
m1 <- merge( books, authors, by.x = "name", by.y = "surname"
           , all.x=FALSE, all.y=FALSE, trace = TRUE)
dim(m1)
unique(paste(m1$name[], m1$nationality[]))
unique(paste(m1$name[], m1$deceased[]))
m2 <- merge( books[,], authors[,], by.x = "name", by.y = "surname"
           , all.x=FALSE, all.y=FALSE, sort = FALSE)
dim(m2)
unique(paste(m2$name[], m2$nationality[]))
unique(paste(m2$name[], m2$deceased[]))
## Left outer join
m1 <- merge( books, authors, by.x = "name", by.y = "surname"
           , all.x=TRUE, all.y=FALSE, trace = TRUE)
class(m1)
dim(m1)
names(books)
names(m1)
unique(paste(m1$name[], m1$nationality[]))
unique(paste(m1$name[], m1$deceased[]))
## Show coercion to allow NA's
authors$test <- ff(TRUE, length=nrow(authors), vmode = "boolean")
vmode(authors$test)
m1 <- merge( books, authors, by.x = "name", by.y = "surname"
           , all.x=TRUE, all.y=FALSE, trace = TRUE)
vmode(m1$test)
table(m1$test[], exclude=c())
options(ffbatchbytes = oldffbatchbytes)
}
\seealso{
  \code{\link{merge}}
}

