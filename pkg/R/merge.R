#' Merge two ffdf by common columns, or do other versions of database join operations. 
#'
#' Merge two ffdf by common columns, or do other versions of database join operations. \cr
#' This method is similar as \code{merge} in the base package but only allows inner joins.
#'
#' @method merge ffdf
#' @export
#' @example ../examples/merge.R
#' @param x an ffdf
#' @param y an ffdf
#' @param by specifications of the common columns. Columns can be specified by name, number or by a logical vector.
#' @param by.x specifications of the common columns of the x ffdf, overruling the by parameter
#' @param by.y specifications of the common columns of the y ffdf, overruling the by parameter
#' @param all see \code{\link{merge}} in R base
#' @param all.x if TRUE, then extra rows will be added to the output, one for each row in x that has no matching row in y. 
#' These rows will have NAs in those columns that are usually filled with values from y. The default is FALSE, so that only rows with data from both x and y are included in the output. 
#' @param all.y similar as all.x
#' @param suffixes character(2) specifying the suffixes to be used for making non-by names() unique.
#' @param incomparables values which cannot be matched. See \code{\link{match}}. Currently not used.
#' @param trace logical indicating to show on which chunk the function is computing
#' @param ... other options passed on to \code{\link[ff]{ffdfindexget}}
#' @return 
#' an ffdf
#' @seealso \code{\link{merge}}
merge.ffdf <- function(x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by, all=FALSE, all.x = all, all.y = all, suffixes = c(".x",".y"), 
		incomparables=NULL, 
		trace=TRUE, ...){
	if (!is.null(incomparables)){
    .NotYetUsed("incomparables != NULL")
  }
  if((all.x == TRUE & all.y == TRUE) | (all.y == TRUE & all.x == TRUE)){
  	stop("merge.ffdf only allows inner joins")
  }
  ## First find the first match where the left hand side ffdf is in the right hand side ffdf
  if(length(by.x) == 1){
  	matchidx <- eval(ffmatch(x[[by.x]], y[[by.y]], trace = trace))
  }else{
  	matchidx <- eval(ffmatch(x[by.x], y[by.y], trace = trace))
  }  
  if(all.x == FALSE & all.y == FALSE){  	
	  jointype <- "inner"
  }else if(all.x == TRUE & all.y == FALSE){
    jointype <- "left"
  	if(any(is.na(matchidx)) == FALSE){
  		jointype <- "inner"
  	}
  }
  if(jointype == "inner"){
  	## Everything from the left is matched, we don't need to change the vmode
  	res <- ffdfindexget(y, index=matchidx, ...)
  	colnames(res) <- ifelse(colnames(res) %in% colnames(x), paste(colnames(res), suffixes[2], sep=""), colnames(res))  	
  }else if(jointype == "left"){
  	## We need to fix the vmode as we are adding NA's
  	stop("merge.ffdf currently does not support left outer joins")
  }
  # cbind it to the existing ffdf
  for(measure in colnames(res)){
  	x[[measure]] <- res[[measure]]
  }  
	x
}
