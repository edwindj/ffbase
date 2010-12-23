#' Minimum method for ff, behaviour is equal to normal min
#'
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