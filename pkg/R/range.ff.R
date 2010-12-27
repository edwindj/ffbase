#' range over ff vector
#'
#' @method range ff
#' @export
#' @param x ff vector
#' @return int vector declaring range
range.ff <- function(x, ..., from=1, to=length(x)){
   range(sapply(chunk(x, from=from, to=to)
               , function(i){
			        range(x[i],...)
			     }
			   )
		, ...
		)
}
