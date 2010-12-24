#' @method min ff
#' @export
min.ff <- function(x, ...){
	min(sapply( chunk(x)
	          , function(i){
					min(x[i], ...)
	            }
	          )
	   , ...
	   )
}