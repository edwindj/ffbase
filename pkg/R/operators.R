#' Ops for ff vectors
#'
#' @rdname ff_vectoroperators
#' @param e1 a \code{ff} vector
#' @param e2 a \code{ff} vector
#' @param ... other parameters passed on to chunk 
#' @seealso \code{\link{groupGeneric}}
">.ff_vector" <- function(e1, e2, ...) {
	res <- NULL
	for (i in chunk(e1, ...)){
		if(inherits(e2, "ff_vector")){
			res <- ffappend(res, e1[i] > e2[i]) 
		}else{
			res <- ffappend(res, e1[i] > e2) 
		}
  }
	res
}
