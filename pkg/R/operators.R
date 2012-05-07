#' Ops for ff vectors
#'
#' @rdname ff_vectoroperators
#' @usage TODO
#' @aliases >.ff_vector


#' @rdname ff_vectoroperators
#' @export 
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
