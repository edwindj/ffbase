#' Duplicated for ffdf objects
#'
#' Duplicated for ffdf objects similar as in \code{\link[base]{duplicated}}.\cr
#' Remark that this duplicated function is slightly different from the duplicated method in the base package as it first orders
#' the ffdf and then applies duplicated. This means you need to order the ffdf in case you want to have the exact same result
#' as the result of the base package. See the example.
#'
#' @export
#' @method duplicated ffdf
#' @example ../examples/duplicated.R
#' @param x a \code{ffdf} object
#' @param incomparables a vector of values that cannot be compared. 
#'   FALSE is a special value, meaning that all values can be compared, 
#'   and may be the only value accepted for methods other than the default. 
#'   It will be coerced internally to the same type as x.
#' @param fromLast logical indicating if duplication should 
#'   be considered from the last, i.e., the last (or rightmost) of identical elements will be kept
#' @param trace logical indicating to show on which chunk the function is computing
#' @param ... other parameters passed on to chunk
#' @return A logical ff vector of length \code{nrow(x)} indicating if each row is duplicated.
#' @seealso \code{\link[base]{duplicated}, \link[ff]{ffdforder}, \link[ff]{fforder}}
duplicated.ffdf <- function(x, incomparables = FALSE, fromLast=FALSE, trace=FALSE, ...){
  if (!identical(incomparables, FALSE)){
    .NotYetUsed("incomparables != FALSE")
  }     
  ## Order the ffdf    
  xorder <- ffdforder(x, decreasing = fromLast, na.last = TRUE)
  xchunk <- chunk(x, ...)
  ## Chunkwise checking if the data is duplicated
  res <- NULL
  lastrow <- NULL
  for (i in xchunk){
    if (trace){
      message(sprintf("%s, working on x chunk %s:%s", Sys.time(), min(i), max(i)))
    }
    iorder <- xorder[i]
    xi <- x[iorder, ]
    xidup <- duplicated(xi)
    ## first one should also be different from the lastrow
    if(sum(duplicated(rbind(xi[1, , drop=FALSE], lastrow)))>0){
      xidup[1] <- TRUE
    }   
    if(length(xidup) > 0){   
      ## Add the result to an ff_vector
      lastrow <- xi[nrow(xi), , drop=FALSE]
      res <- ffappend(x=res, y=xidup)        
    }
  }
  ## Put res back in the right order
  finalres <- NULL
  xorderreversed <- fforder(xorder)
  xorderreversedchunk <- chunk(xorderreversed, ...)
  for (i in xorderreversedchunk){
    if (trace){
      message(sprintf("%s, working on xorderreversed chunk %s:%s", Sys.time(), min(i), max(i)))
    }
    finalres <- ffappend(x=finalres, y=res[i])
  }
  finalres
}
