#' Date Conversion Functions for \code{ff} vector
#' 
#' Date Conversion Functions for \code{ff} vector.
#'
#' @method as.Date ff
#' @aliases as.Date.ff format.ff
#' @param x an object of class \code{ff_vector}
#' @param ... other parameters passed on to as.Date
#' @param inplace passed on to \code{\link{chunkify}}
#' @return An \code{ff_vector} of length(x) containing the result of as.Date/format applied to the elements in chunks
as.Date.ff <- chunkify(fun = as.Date)

#' @method format ff
#' @rdname as.Date.ff
format.ff <- chunkify(fun = format)

