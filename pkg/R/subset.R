#' @method subset ff
#' @export
#' @param x ff vector to be subset
#' @return a new ff vector containing the subset, data is physically copied
subset.ff <- function(x, subset, ...){
	#y <- ff(length=sum(subset), vmode=vmode(x))
	y <- clone(x)
	length(y) <- sum(subset, na.rm=TRUE)
	y[] <- x[subset]
	y
}

#' @method subset ffdf
#' @export
#' @param x ffdf data frame
#' @return a new ffdf data.frame containing the subset, data is physically copied
subset.ffdf <- function(x, subset, ...){
	y <- clone(x)
	n <- 0
	#subset <- eval(expression(substitute(subset)))
	#print(subset)
	#TODO check if subset is an expression or logical vector. If so, then idx vector
	for (i in chunk(x)){
	   dat <- x[i,]
	   row.names(dat) <- min(i):max(i)
	   sel <- eval(substitute(subset), dat, parent.frame())
	   dat <- dat[sel,]
	   s <- nrow(dat) 
	   if (s > 0){
	      y[(n+1):(n+s),] <- dat
		  n <- n + s
	   }
	}
	nrow(y) <- n
	y 
}