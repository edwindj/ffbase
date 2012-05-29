#' Concatenate \code{ff} vectors
#' 
#' @export
#' @seealso \code{\link{ffappend}}
#' @method c ff
#' @param ... \code{ff} ff vectors to be concatenated
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
#' @param ... parameter that will be passed on to \code{chunk} interally 
#' @return \code{ff} object with same physical storage as \code{x}
#' @export
ffappend <- function(x, y, ...){
   if (is.null(x)){
      if (is.ff(y)){
		    return(clone(y))
	    } else {
        return (if (length(y)) as.ff(y))
	    }
   }
   #TODO check if x and y are compatible
   len <- length(x)
   to <- length(y)
   if (!to) return(x)
   
   length(x) <- len + to
   
   if (is.factor(x)){
      levels(x) <- appendLevels(levels(x), levels(y))  
   }
   
   for (i in chunk(x, from=1, to=to, ...)){
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
#' @param x \code{ffdf} object where data will be appended to. If \code{x==NULL} a new \code{ffdf} object will be created
#' @param dat \code{ffdf} object or \code{data.frame} object
#' @param recode should factors be recoded (default), or not (faster)
#' @param ... Further arguments passed to \code{\link{as.ffdf}}, when \code{x==NULL}
#' @return \code{ffdf} object with same physical storage as \code{x}
ffdfappend <- function(  x
                       , dat
                       , recode=TRUE
                       , ...
                       ){
  
  fc <- if (is.ffdf(dat)){ 
    sapply(physical(dat), function(i) is.factor(i) || is.character(i))
  } else {
    sapply(dat, function(i) is.factor(i) || is.character(i))    
  }
  
  if (any(fc)){
    dat[fc] <- lapply( which(fc), function(i) {
        as.factor(dat[[i]])
    })
  }
  
  if (is.null(x)){
      return(as.ffdf(dat))
  }
     
   n <- nrow(dat)
   nff <- nrow(x)
   
   nrow(x) <- nff + n
  
   for (i in which(fc)){
     levels(x[[i]]) <- appendLevels(levels(x[[i]]), dat[[i]])
   }
   if(!identical(names(x), names(dat))){ 
   	warning("names not identical when appending 2 ffdf's")
   }
   
   i <- hi(nff+1, nff+n)
   x[i,] <- dat
   
   x
}
