#' Unique values
#'
#' @export
#' @method unique ff
#' @param x \code{ff} object
#' @param incomparables a vector of values that cannot be compared. 
#'   FALSE is a special value, meaning that all values can be compared, 
#'   and may be the only value accepted for methods other than the default. 
#'   It will be coerced internally to the same type as x.
#' @param fromLast logical indicating if duplication should 
#'   be considered from the last, i.e., the last (or rightmost) of identical elements will be kept
#' @param ... Further arguments passed to \code{\link{unique}}
#' @return vector with unique values of \code{x}
unique.ff <- function(x, incomparables = FALSE, fromLast = FALSE, ...){
   #TODO make a version that results in a ff vector
   z <- NULL
   for (i in chunk(x, ...)){
      z <- unique(c(z,x[i]), incomparables, fromLast, ...)
   }
   z
}