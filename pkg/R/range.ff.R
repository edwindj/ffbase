#' @method range ff
#' @export
range.ff <- function(x, ...){
   range(sapply(chunk(x)
               , function(i){
			        range(x[i],...)
			     }
			   )
		, ...
		)
}
