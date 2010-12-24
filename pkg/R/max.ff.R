#' @method max ff
#' @export
max.ff <- function(x, ...){
	max(sapply( chunk(x)
	          , function(i){
					max(x[i], ...)
	            }
	          )
	   , ...
	   )
}
