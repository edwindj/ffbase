#' Add a ff vector that contains the order of the \code{\link{ff}} vector \code{x}
#' 
#' Add a ff vector that contains the order of the \code{\link{ff}} vector \code{x}
#' The index can be retrieved using \code{ffindex}. Note that you have to assign the result to the
#' original vector \code{x}.
#' @example ../examples/ffindex.R
#' @param x \code{ff} vector to be indexed
#' @param addKey should the sorted values also be stored in \code{ffkey}?
#' @param ... parameters that will be passed on to \code{\link{fforder}}. 
#' @return The updated vector \code{x}
#' @rdname ffindex
#' @export addffIndex ffindex ffkey
addffIndex <- function(x, addKey=FALSE, ...){
  index <- fforder(x)
  filename(index) <- sub("(\\.ff)?$", ".ffindex", filename(x))
  attr(x, "ffindex") <- index
  if (addKey){
     key <- x[index]
     is.sorted(key) <- TRUE
     filename(key) <- sub("(\\.ff)?$", ".ffkey", filename(x))
     attr(x, "ffkey") <- key
  }
  x
}

#' @rdname ffindex
ffindex <- function(x){
  idx <- attr(x, "ffindex")
  if (is.null(idx)){
    fforder(x)
  } else {
    idx
  }
}

#' @rdname ffindex
ffkey <- function(x){
  attr(x, "ffkey")
}

# ### Quick testing
# x <- ff(runif(10))
# x <- addffIndex(x, addKey=TRUE)
# x[]
# ffindex(x)[]
# ffkey(x)[]