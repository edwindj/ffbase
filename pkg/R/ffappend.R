#' Concatenate \code{ff} vectors
#' 
#' @export
#' @seealso \code{link{ffappend}}
#' @method c ff
#' @param ... \code{ff} ff vectors to be combined
#' @return a new \code{ff} object, data is physically copied
c.ff <- function(...){
   l <- list(...)
   f <- NULL
   for (x in l){
      f <- ffappend(f, x)
   }
   f
}

#' Append a ff vector to another ff vector
#' 
#' Appends (\code{ff}) vector \code{y} to \code{ff} vector \code{x}. 
#' Please note that the data of \code{y} will be coerced to the type of \code{x}.
#' @seealso \code{\link{c.ff}}
#' @param x \code{ff} object where data will be appended to. If \code{x==NULL} a new \code{ff} object will be created
#' @param y \code{ff} object or vector object
#' @return \code{ff} object with same physical storage as \code{x}
ffappend <- function(x, y){
   if (is.null(x)){
      if (is.ff(y)){
		return(clone(y))
	  }
	  else {
		return(as.ff(y)) 
	  }
   }
   #TODO check if x and y are compatible
   len <- length(x)
   length(x) <- len + length(y)
   for (i in chunk(x, from=1, to=length(y))){
       if (is.vector(y)){
			i <- as.which(i)
	   }
	   x[(i+len)] <- y[i]
   }
   x
}

#' append a dataframe to a ffdf
#' 
#' Appends (\code{ff}) vector to \code{ff} vector \code{x}. 
#' Please note that the data of \code{y} will be coerced to the type of \code{x}.
#' @seealso \code{\link{c.ff}}
#' @param x \code{ffdf} object where data will be appended to. If \code{x==NULL} a new \code{ff} object will be created
#' @param dat \code{ffdf} object or \code{data.frame} object
#' @param recode should factors be recoded (default), or not (faster)
#' @param ... Further arguments passed to \code{\link{as.ffdf}}, when \code{x==NULL}
#' @return \code{ffdf} object with same physical storage as \code{x}
ffdfappend <- function(  x
                       , dat
                       , recode=TRUE
                       , ...
                       ){
   if (is.null(x)){
      return(as.ffdf(dat, ...))
   }   
   
   #TODO add checks if structure x and dat are equal
   if (recode){
   }
   
   n <- nrow(dat)
   nff <- nrow(x)
   
   #it is not possible to have an empty ffdf so this trick lets
   if (nff==1) {nff<-0} 
   
   nrow(x) <- nff + n
   i <- hi(nff+1, nff+n)
   x[i,] <- dat
   
   x
}