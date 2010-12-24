#' @method sum ff
#' @export
sum.ff <- function(x, ...){
   sum( sapply( chunk(x)
              , function(i){
	               sum(x[i], ...)
	            }
	          , ...
	          )
	   )
}
