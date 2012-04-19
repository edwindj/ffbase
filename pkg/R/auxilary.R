checkRange <- function(range, x){
    
    if (is.null(range)){
       return(ri(1,length(x)))
    }
     
    #TODO add checks
    range
}

#' Groups the input integer vector into several groups if the running cumulative sum increases a certain maximum number
#'
#' Groups the input integer vector into several groups if the running cumulative sum increases a certain maximum number
#'
#' @useDynLib ffbase
#' @param x an integer vector
#' @param max the maximum running cumulative size before an extra grouping is done
#' @return An integer vector of the same length of x, indicating groups
grouprunningcumsum <- function(x, max){
	l <- as.integer(length(x))
	if(l == 0){
		return(x)
	}
	x <- as.integer(x)	
	max <- as.integer(max)
	result <- .C("grouprunningcumsum",
			x = x, 
			l = l, 
			max = max,
			PACKAGE="ffbase")
	result$x
}
