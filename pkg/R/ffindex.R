#' Add a ff vector that contains the order of the \code{\link{ff}} vector \code{x}
#' 
#' The index can be retrieve using \code{ffindex}. Note that you have to assign the result to the
#' Original vector \code{x}.
#' @param x \code{ff} vector to be indexed
#' @param addKey should the sorted values also be stored in.
#' @param ... parameters that will be passed on to \code{\link{fforder}}. 
#' @return The updated vector \code{x}
#' @rdname ffindex
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
  attr(x, "ffindex")
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