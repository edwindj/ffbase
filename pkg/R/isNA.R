#' 'Not Available' / Missing Values for ff vectors
#'
#' The generic function \code{is.na} indicates which elements are missing.\cr
#' The generic function \code{is.na<-} sets elements to \code{NA}. 
#'
#' @rdname is.na.ff
#' @method is.na ff 
#' @method is.na<-
#' @aliases {is.na<-.ff}
#' @example ../examples/isNA.R
#' @param x a \code{ff} vector
#' @param value a suitable ff index vector for use with x
#' @param ... other parameters passed on to ffvecapply except VMODE and RETURN in case of is.na.ff and to chunk in case of is.na<-
#' @return An logical \code{ff} vector of the same length of x indicating if the ff vector contains missing values. 
#' @export is.na.ff is.na<-.ff
#' @seealso \code{\link[base]{is.na}, \link[ff]{ffvecapply}}
is.na.ff <- function(x, ...){
	ffvecapply(x[i1:i2] <- is.na(x[i1:i2]), X = x, VMODE = "boolean", RETURN = TRUE, ...)
}

#' @rdname is.na.ff
"is.na<-.ff" <- function(x, ..., value){
	if(inherits(value, "ff_vector")){
		for (i in chunk(value, ...)){
  		set.ff(x=x, i=value[i], value=NA, add = FALSE)
  	}	
	}else{
		set.ff(x=x, i=value, value=NA, add = FALSE)
	}	
	x	
}


