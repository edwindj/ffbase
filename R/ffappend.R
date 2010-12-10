#append a dataframe to a ffdf

ffappend <- function( x
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