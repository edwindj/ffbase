#' Unique values
#'
#' @export
#' @method unique ff
#' @param x \code{ff} object
#' @param incomparables a vector of values that cannot be compared. 
#'   FALSE is a special value, meaning that all values can be compared, 
#'   and may be the only value accepted for methods other than the default. 
#'   It will be coerced internally to the same type as x.
#' @param fromLast logical indicating if duplication should 
#'   be considered from the last, i.e., the last (or rightmost) of identical elements will be kept
#' @param ... Further arguments passed to \code{\link{unique}}
#' @return vector with unique values of \code{x}
unique.ff <- function(x, incomparables = FALSE, fromLast = FALSE, ...){
   #TODO make a version that results in a ff vector
   z <- NULL
   for (i in chunk(x, ...)){
      z <- unique(c(z,x[i]), incomparables, fromLast, ...)
   }
   z
}

#' Unique for ffdf objects
#'
#'
#' @export
#' @method unique ffdf
#' @example ../examples/unique.R
#' @param x a \code{ffdf} object
#' @param incomparables a vector of values that cannot be compared. 
#'   FALSE is a special value, meaning that all values can be compared, 
#'   and may be the only value accepted for methods other than the default. 
#'   It will be coerced internally to the same type as x.
#' @param fromLast logical indicating if duplication should 
#'   be considered from the last, i.e., the last (or rightmost) of identical elements will be kept
#' @param trace logical indicating to show on which chunk the function is computing
#' @param ... other parameters passed on to chunk
#' @return An ffdf with unique values in \code{x}. 
#' @seealso \code{\link[base]{unique}}
unique.ffdf <- function(x, incomparables = FALSE, fromLast=FALSE, trace=FALSE, ...){
  if (!identical(incomparables, FALSE)){
    .NotYetUsed("incomparables != FALSE")
  }     
  ## Order the ffdf    
  xorder <- ffdforder(x, decreasing = fromLast, na.last = TRUE)
  xchunk <- chunk(x, ...)
  ## Chunkwise adding of unique rows to the unique ffdf called res
  res <- NULL
  lastrow <- NULL
  for (i in xchunk){
    if (trace){
      message(sprintf("%s, working on x chunk %s:%s", Sys.time(), min(i), max(i)))
    }
    iorder <- xorder[i]
    xi <- x[iorder, ]
    xi <- unique(xi)
    rownames(xi) <- NULL
    ## exclude the first row if it was already in the unique ffdf as this is the last one from the previous unique
    if(sum(duplicated(rbind(xi[1, , drop=FALSE], lastrow)))>0){
      xi <- xi[-1, , drop=FALSE]  
    }   
    if(nrow(xi) > 0){   
      ## Add the result to an ffdf
      lastrow <- xi[nrow(xi), , drop=FALSE]
      if(!is.null(res)){
        rownames(xi) <- (nrow(res)+1):(nrow(res)+nrow(xi))
      }
      res <- ffdfappend(x=res, dat=xi, recode=FALSE)        
    }
  }
  res
}
