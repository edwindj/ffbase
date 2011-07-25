#' Chunkify an element-wise function
#' 
#' Chunkify creates a new function that operates on a ff vector. 
#' It creates chunks from the ff vector and calls the orginal function \code{fun} on each chunk.
#' @export chunkify
#' @param fun function to be 'chunkified', the function must accept a vector and 
#'    return a vector of the same \code{length}
#' @return 'chunkified' function that accepts a \code{ff} vector as its first argument.
chunkify <- function(fun){
   cfun <- function( x
                   , ...
                   , inplace=FALSE
				       ){
                   
     chunks <- chunk(x)
     
     i <- chunks[[1]]
     ret <- as.ff(fun(x[i], ...))
     length(ret) <- length(x)
     
     for (i in chunks[-1]){
	     ret[i] <- fun(x[i], ...)
	  }
	  ret
   }
   cfun
}