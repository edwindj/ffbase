#' Cumulative Sums, Products, and Extremes
#'
#' @method cumsum ff
#' @example ../examples/cumsum.R
#' @param x \code{ff} numeric vector or an object that can be coerced to one a numeric vector
#' @param ... other parameters passed on to chunk
#' @return An \code{ff} vector of the same length and type as x (after coercion), except that cumprod returns a numeric vector for integer input.\cr
#' An NA value in x causes the corresponding and following elements of the return value to be NA, as does integer overflow in cumsum (with a warning). 
#' @rdname cumsum.ff
#' @export cumsum.ff cumprod.ff cummax.ff cummin.ff
#' @seealso \code{\link{cumsum}}, \code{\link{cumprod}}, \code{\link{cummax}}, \code{\link{cummin}}
cumsum.ff <- function(x, ...){
	result <- NULL
  for (i in chunk(x, ...)){
  	firstidx <- i[1]
    if(firstidx == 1){
    	running <-  cumsum(x[i])
    	result <- ffappend(result, running)
    	lastelement <- running[length(running)]
    }else{
    	running <- cumsum(c(lastelement, x[i]))
    	result <- ffappend(result, running[-1])
    	lastelement <- running[length(running)]
    }    
  }
  result
}

#' @rdname cumsum.ff
#' @method cumprod ff
cumprod.ff <- function(x, ...){
	result <- NULL
  for (i in chunk(x, ...)){
  	firstidx <- i[1]
    if(firstidx == 1){
    	running <-  cumprod(x[i])
    	result <- ffappend(result, running)
    	lastelement <- running[length(running)]
    }else{
    	running <- cumprod(c(lastelement, x[i]))
    	result <- ffappend(result, running[-1])
    	lastelement <- running[length(running)]
    }    
  }
  result
}

#' @rdname cumsum.ff
#' @method cummax ff
cummax.ff <- function(x, ...){
	result <- NULL
  for (i in chunk(x, ...)){
  	firstidx <- i[1]
    if(firstidx == 1){
    	running <-  cummax(x[i])
    	result <- ffappend(result, running)
    	lastelement <- running[length(running)]
    }else{
    	running <- cummax(c(lastelement, x[i]))
    	result <- ffappend(result, running[-1])
    	lastelement <- running[length(running)]
    }    
  }
  result
}

#' @rdname cumsum.ff
#' @method cummin ff
cummin.ff <- function(x, ...){
	result <- NULL
  for (i in chunk(x, ...)){
  	firstidx <- i[1]
    if(firstidx == 1){
    	running <-  cummin(x[i])
    	result <- ffappend(result, running)
    	lastelement <- running[length(running)]
    }else{
    	running <- cummin(c(lastelement, x[i]))
    	result <- ffappend(result, running[-1])
    	lastelement <- running[length(running)]
    }    
  }
  result
}


