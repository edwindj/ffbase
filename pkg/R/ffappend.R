
# append an ff vector to another ff vector
c.ff <- function(...){
   l <- list(...)
   f <- NULL
   for (x in l){
      f <- ffappend(f, x)
   }
   f
}

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

#append a dataframe to a ffdf
ffdfappend <- function( x
  					  , dat
					  , recode=TRUE
					  , vmode = NULL
					  , col_args = list()
					  , ...
					  ){
   if (is.null(x)){
      return(as.ffdf(dat, vmode, col_args, ...))
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