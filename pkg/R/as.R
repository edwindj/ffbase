#' @title Character Vectors
#'
#' The generic function \code{as.character} converts \code{ff} vectors to characters.\cr
#'
#' @rdname as.character.ff
#' @method as.character ff 
#' @example ../examples/as.R
#' @param x a \code{ff} vector
#' @param ... other parameters passed on to chunk
#' @return A factor \code{ff} vector of the same length of x.
#' @export 
#' @seealso \code{\link[base]{as.character}}
as.character.ff <- function(x, ...){
	levs <- unique(x)[]
	levs <- levs[!is.na(levs)]
	res <- ff(vmode="integer", length = length(x), levels=as.character(levs))
	for (i in chunk(x, ...)){
    Log$chunk(i)
		res[i] <- as.character(x[i])		
	}
	res		
}



as.ff_matrix <- function(x, ...){
  UseMethod("as.ff_matrix")
}
as.ff_matrix.ffdf <- function(x, ...){
  result <- ff(NA, dim = dim(x), vmode = names(maxffmode(vmode(test)))[1])
  dimnames(result) <- dimnames(x)
  for(ichunk in chunk(x)){
    Log$chunk(i)
    result[ichunk, ] <- as.matrix(x[ichunk, ])
  }
  result
}
