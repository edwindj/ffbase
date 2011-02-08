#' Chunkify an element-wise function
#' 
#' @param fun function to be 'chunkified', the function must accept a vector and 
#'    return a vector of the same \code{length}
chunkify <- function(fun){
   cfun <- function( x
                   , ...
                   , inplace=FALSE
				       ){
                   
     chunks <- chunk(x)
     
     i <- chunks[[1]]
     ret <- as.ff(fun(x[i], ...))
     length(ret) <- length(x)
     
     for (i in chunks[-1]){
	     ret[i] <- fun(x[i], ...)
	  }
	  ret
   }
   cfun
}

evalInChunks <- function(e, ...){
   l <- as.list(substitute(list(...)))[-1]
   nm <- names(l)
   fixup <- if (is.null(nm)) 
      seq_along(l)
   else nm == ""
   dep <- sapply(l[fixup], deparse)
   if (is.null(nm)) 
      names(l) <- dep
   else {
      names(l)[fixup] <- dep
   }
   
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