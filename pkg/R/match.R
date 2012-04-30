#' Value Matching for ff objects
#'
#' \code{match} returns an ff vector of the positions of (first) matches of its first argument in its second. 
#' Similar as \code{\link[base]{match}}.
#'
#' @export
#' @param x a \code{ff} object
#' @param table a \code{ff} object
#' @param nomatch the value to be returned in the case when no match is found. Note that it is coerced to \code{integer}.
#' @param incomparables a vector of values that cannot be matched. Any value in \code{x} matching a value in this vector is assigned the nomatch value. For historical reasons, \code{FALSE} is equivalent to \code{NULL}.
#' @param trace logical indicating to show on which chunk the function is computing
#' @param ... other parameters passed on to chunk
#' @return An ff vector of the same length as \code{x}. An integer vector giving the position in table of the first match if there is a match, otherwise \code{nomatch}. 
#' @seealso \code{\link[base]{match}}
ffmatch <- function(x, table, nomatch = NA_integer_, incomparables = NULL, trace=FALSE, ...){
	nomatch <- as.integer(nomatch)
	xchunk <- chunk(x, ...)
	tablechunk <- chunk(table, ...)
	if(trace) message(sprintf("%s, x has %s chunks, table has %s chunks", Sys.time(), length(xchunk), length(tablechunk)))
	allresults <- NULL
	## First work on looping over x, then over the table
	for (i in xchunk){
		if(trace) message(sprintf("%s, working on x chunk %s:%s", Sys.time(), min(i), max(i)))
		lhs <- list()
		lhs$finalresult <- rep(nomatch, (max(i)-min(i)+1))
		lhs$finishedidx <- integer(0)
		lhs$ram <- x[i]			
		for (j in tablechunk){
			if(length(lhs$finishedidx) == length(lhs$finalresult)) next
			if(trace) message(sprintf("%s, working on table chunk %s:%s", Sys.time(), min(j), max(j)))
			rhs <- table[j]
			result <- match(x=lhs$ram, table=rhs, nomatch=nomatch, incomparables=incomparables)
			## which ones are matched - if they were already matched do not use them (see definition: positions of (first) matches)
			lhs$currentmatchidx <- which(!result %in% nomatch)
			lhs$currentmatchidx <- lhs$currentmatchidx[!lhs$currentmatchidx %in% lhs$finishedidx]			
			if(length(lhs$currentmatchidx) > 0){
				result <- result + (min(j) - 1)
				lhs$finishedidx <- unique(c(lhs$finishedidx, lhs$currentmatchidx))
				lhs$finalresult[lhs$currentmatchidx] <- result[lhs$currentmatchidx]
			}			
		}
		allresults <- ffappend(x=allresults, y=lhs$finalresult)
	}	
	allresults
}



