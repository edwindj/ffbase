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

evalInChunks <- function(e, ...){
   args <- list(...)
   if ( length(args) == 1 
     && is.ffdf(args[[1]])
      ){
      dat <- args[[1]]
   }
   else {
      #TODO check if all arguments are ff and of same length
      dat <- do.call(ffdf, args)
      print(dat)
   }
   e <- substitute(e)
   
   chunks <- chunk(dat, by=2)
   
   cdat <- dat[chunks[[1]],,drop=FALSE]
   
   res <- eval(e, cdat)
   if (is.vector(res)){
      res <- as.ff(res)
      length(res) <- nrow(dat)
      for (i in chunks[-1]){
         res[i] <- eval(e, dat[i,,drop=FALSE])
      }
   }
   else if (is.data.frame(res)){
      res <- as.ffdf(res)
      nrow(res) <- nrow(dat)
      for (i in chunks[-1]){
         res[i,] <- eval(e, dat[i,,drop=FALSE])
      }
   }
   res
}