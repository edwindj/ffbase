
# append an ff vector to another ff vector

ffappend <- function(x, y){
   if (is.null(x)){
      return clone(y)
   }
   #TODO check if x and y are compatible
   len <- length(x)
   length(x) <- len + length(y)
   x[hi(len+1, length(x))] <- y
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
      return as.ffdf(dat, vmode, col_args, ...)
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