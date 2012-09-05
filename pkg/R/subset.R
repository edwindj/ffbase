#' Subsetting a ff vector or ffdfdata frame
#'
#' @export subset.ff subset.ffdf
#' @aliases subset.ff subset.ffdf
#' @method subset ff
#' @param x \code{ff} vector or \code{ffdf} data.frame to be subset
#' @param subset an expression, \code{ri}, \code{bit} or logical \code{ff} vector that can be used to index x
#' @param ... not used
#' @return a new ff vector containing the subset, data is physically copied
subset.ff <- function(x, subset, ...){
	#y <- ff(length=sum(subset), vmode=vmode(x))
	y <- clone(x)
	length(y) <- sum(subset, na.rm=TRUE)
   #TODO fix this for very large subsets...
	y[] <- x[subset]
	y
}

subset.ffdf <- function(x, subset, select, ...){
  ss <- as.expression(substitute(subset))
  try(ss <- subset, silent=TRUE)
  idx <- ffwhich(x, ss)
  x[idx,select]
}


# quick testing
# x <- as.ffdf(iris)
# log <- x$Species == "setosa"
# 
# subset(x, Species=="setosa")
#ffwhich(x, log)