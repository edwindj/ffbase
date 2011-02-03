chunkify <- function(fun, vmode=NULL){
   vm <- vmode
   cfun <- function( x
                   , ...
                   , inplace=FALSE
                   , vmode = if (is.null(vm)) vmode(x)
                             else vm
				       ){
     ret <- ff(length=length(x), vmode=vmode)
	  for (i in chunk(ret)){
	     ret[i] <- fun(x[i], ...)
	  }
	  ret
   }
   cfun
}

evalInChunks <- function(dat, e){
   e <- substitute(e)
   
   chunks <- chunk(dat)
   
   cdat <- dat[chunks[[1]],,drop=FALSE]
   res <- eval(e, cdat)
   if (is.vector(res)){
      res <- as.ff(res)
      length(res) <- nrow(dat)
      for (i in chunks[-1]){
         res[i] <- eval(dat[i], e)
      }
   }
   else if (is.data.frame(res)){
      res <- as.ffdf(res)
      nrow(res) <- nrow(dat)
   }
   for (i in chunks[-1]){
      res[i,] <- eval(dat[i], e)
   }
   res
}