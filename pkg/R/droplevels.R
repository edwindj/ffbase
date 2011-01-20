#' The function \code{droplevels} is used to drop unused levels from a \code{ff} factor or
#' , more commonly, from factors in a \code{ffdf}
#' @export
#' @method droplevels ff 
droplevels.ff <- function(x, ..., inplace=FALSE){
   if (!is.factor(x)){
		stop("droplevels can only applied to a factor")      
   }
   
   if (!inplace){
      x <- clone(x)
   }
   
   levs <- levels(x)
   used <- logical(length(levs))
   for (i in chunk(x)){
      used <- used | (levs %in% x[i])
   }
   recodeLevels(x, levs[used])
}


droplevels.ffdf <- function(x, except=NULL, ..., inplace=FALSE){
   ffs <- physical(x)
   ix <- sapply(ffs, is.factor)
   if (!is.null(except)) 
        ix[except] <- FALSE
	
   ffs[ix] <- lapply(ffs[ix], droplevels.ff, ..., inplace)
   do.call("ffdf", ffs)
}