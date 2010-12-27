#' Summing an ff vector
#'
#' @usage
#' @method sum ff
#' @export
sum.ff <- function(x, na.rm=FALSE, ..., from=1, to=length(x)){
   sum( sapply( chunk(x, from=from, to=to)
              , function(i){
	               sum(x[i], na.rm, ...)
	            }
	          , ...
	          )
	   ) 
}