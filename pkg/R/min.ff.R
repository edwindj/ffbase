#' Minimum of ff vector
#'
#' @method min ff
#' @export
min.ff <- function(x, from=1, to=length(x), ...){
	min(sapply( chunk(x, from=from, to=to)
	          , function(i){
					min(x[i], ...)
	            }
	          )
	   , ...
	   )
}