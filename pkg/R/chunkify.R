#' Chunkify an element-wise function
#' 
#' Chunkify creates a new function that operates on a ff vector. 
#' It creates chunks from the ff vector and calls the orginal function \code{fun} on each chunk.
#' @export chunkify
#' @param fun function to be 'chunkified', the function must accept a vector and 
#'    return a vector of the same \code{length}
#' @return 'chunkified' function that accepts a \code{ff} vector as its first argument.
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

## change to names
chunkexpr <- function(x, expr){
  es <- deparse(expr[[1]])
  xs <- deparse(x[[1]])
  print(list(es=es, xs=xs))  
  if (is.ff(x)){
      varre <- paste("\\b(",xs,")\\b", sep="")
      es <- gsub(varre, "\\1[i]", es)

  } else if (is.ffdf(x)){  
      for (var in names(x)){
        varre <- paste("\\b(",var,")\\b", sep="")
        varsub <- paste(xs,"$\\1[i]", sep="")
        es <- gsub(varre, varsub, es)
        #print(list(varre=varre, varsub=varsub, es=es))
      }
  }
  parse(text=es)
}

#chunkexpr(expression(x), expression(x>2))
