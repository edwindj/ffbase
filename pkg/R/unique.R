#' Unique values
#'
#' @export
#' @method unique ff
#' @param x \code{ff} object
#' @param incomparables ?
#' @param fromLast ?
#' @param ... Further arguments passed to \code{\link{unique}}
#' @return vector with unique values of \code{x}
unique.ff <- function(x, incomparables = FALSE, fromLast = FALSE, ...){
   #TODO make a version that results in a ff vector
   z <- NULL
   for (i in chunk(x)){
      z <- unique(c(z,x[i]), incomparables, fromLast, ...)
   }
   z
}