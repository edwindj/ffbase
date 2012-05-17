#' Duplicated for ff and ffdf objects
#'
#' Duplicated for ff and ffdf objects similar as in \code{\link[base]{duplicated}}.\cr
#' Remark that this duplicated function is slightly different from the duplicated method in the base package as it first orders
#' the ffdf or ff_vector object and then applies duplicated. This means you need to order the ffdf or ff_vector 
#' in case you want to have the exact same result as the result of the base package. See the example.
#'
#' @rdname duplicated.ff
#' @export duplicated.ff duplicated.ffdf
#' @method duplicated ff
#' @example ../examples/duplicated.R
#' @param x \code{ff} object or \code{ffdf} object
#' @param incomparables a vector of values that cannot be compared. 
#'   FALSE is a special value, meaning that all values can be compared, 
#'   and may be the only value accepted for methods other than the default. 
#'   It will be coerced internally to the same type as x.
#' @param fromLast logical indicating if duplication should 
#'   be considered from the last, i.e., the last (or rightmost) of identical elements will be kept
#' @param trace logical indicating to show on which chunk the function is computing
#' @param ... other parameters passed on to chunk
#' @return A logical ff vector of length \code{nrow(x)} or \code{length(x)} indicating if each row or element is duplicated.
#' @seealso \code{\link[base]{duplicated}, \link[ff]{ffdforder}, \link[ff]{fforder}}
duplicated.ff <- function(x, incomparables = FALSE, fromLast=FALSE, trace=FALSE, ...){
  if (!identical(incomparables, FALSE)){
    .NotYetUsed("incomparables != FALSE")
  }     
  finalres <- ff(vmode="logical", length=length(x))
  ## Order the ffdf    
  xorder <- fforder(x, decreasing = fromLast, na.last = TRUE)
  xchunk <- chunk(x, ...)
  ## Chunkwise checking if the data is duplicated
  res <- NULL
  lastel <- NULL
  for (i in xchunk){
    if (trace){
      message(sprintf("%s, working on x chunk %s:%s", Sys.time(), min(i), max(i)))
    }
    iorder <- xorder[i]
    xi <- x[iorder]
    xidup <- duplicated(xi)
    ## first one should also be different from the lastel
    if(sum(duplicated(c(xi[1], lastel)))>0){
      xidup[1] <- TRUE
    }   
    if(length(xidup) > 0){   
      ## Add the result to an ff_vector
      lastel <- xi[length(xi)]
      res <- ffappend(x=res, y=xidup)        
    }
  }
  ## Put res back in the right order
  xorderreversed <- fforder(xorder)
  xorderreversedchunk <- chunk(xorderreversed, ...)
  for (i in xorderreversedchunk){
    if (trace){
      message(sprintf("%s, working on xorderreversed chunk %s:%s", Sys.time(), min(i), max(i)))
    }
    finalres[i] <- res[i]
  }
  finalres
}


#' @rdname duplicated.ff
#' @method duplicated ffdf
duplicated.ffdf <- function(x, incomparables = FALSE, fromLast=FALSE, trace=FALSE, ...){
  if (!identical(incomparables, FALSE)){
    .NotYetUsed("incomparables != FALSE")
  }     
  finalres <- ff(vmode="logical", length=nrow(x))
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
  xorderreversed <- fforder(xorder)
  xorderreversedchunk <- chunk(xorderreversed, ...)
  for (i in xorderreversedchunk){
    if (trace){
      message(sprintf("%s, working on xorderreversed chunk %s:%s", Sys.time(), min(i), max(i)))
    }
    finalres[i] <- res[i]
  }
  finalres
}




