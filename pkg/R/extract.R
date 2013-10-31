#' Reading and writing vectors extended to handle logical \code{ff} vectors as indexes
#'
#' Package \code{ff} does not allow to extract and set values of \code{ff} vectors based on logical \code{ff} vectors. For this reason
#' the extractor functions \code{[.ff} and \code{[<-.ff} defined in package ff are overloaded.\cr
#' If you supply a logical \code{ff} vector as an index to another \code{ff} vector, the overloaded function will convert it to an integer \code{ff}. 
#' index before using the \code{[.ff} and \code{[<-.ff} function from the ff package. \cr
#' This allows to do \code{ff(1:10)[ff(c(FALSE, TRUE, NA, TRUE))]}\cr\cr
#' Mark that all other functionality from the extractor functions \code{[.ff} and \code{[<-.ff} in package ff are retained. This is an extension 
#' to handle logical \code{ff} vectors as indexes.  
#'
#' @export
#' @rdname ffextract
#' @example ../examples/extract.R
#' @param x an \code{ff} object 
#' @param i missing OR a single index expression OR a \code{\link[ff]{hi}} object 
#' @param add TRUE if the values should rather increment than overwrite at the target positions, see \code{\link[ff]{readwrite.ff}}
#' @param pack FALSE to prevent rle-packing in hybrid index preprocessing, see \code{\link[ff]{as.hi}}
#' @param value the values to be assigned, possibly recycled
#' @usage \method{[}{ff} (x, i, pack = FALSE)
#' @return See \code{\link[ff]{Extract.ff}}. Mark that if a logical \code{ff} vector is used for \code{i}, and if only \code{FALSE} or \code{NA} 
#' values are present, NULL is returned in case of the extractor function \code{[.ff} while for the setter function \code{[<-.ff}, if the length value
#' is zero, this is not allowed.
#' @seealso \code{\link[ff]{Extract.ff}}
"[.ff" <- function(x, i, pack = FALSE){
  if(!missing(i) && is.ff(i) && length(i) > 0 && is.logical(i[1])){
    idx <- ffwhich(i, i==TRUE)    
    if(length(idx) == 0){
      warning("you are indexing ff vectors which return zero length, do you really need to use ff?")
      return(NULL)
    }    
    finalizer(idx) <- "delete"
    ff::`[.ff`(x=x, i=idx, pack=pack)
  }else{
    ff::`[.ff`(x=x, i=i, pack=pack)
  }	
}

#' @rdname ffextract
#' @usage \method{[}{ff} (x, i, add = FALSE, pack = FALSE) <- value
#' @export
"[<-.ff" <- function(x, i, add = FALSE, pack = FALSE, value){
  if(!missing(i) && is.ff(i) && length(i) > 0 && is.logical(i[1])){
    idx <- ffwhich(i, i==TRUE)    
    if(length(idx) == 0){
      stop("no value for replacement")
    }    
    finalizer(idx) <- "delete"
    ff::`[<-.ff`(x=x, i=idx, add=add, pack=pack, value=value)
  }else{
    ff::`[<-.ff`(x=x, i=i, add=add, pack=pack, value=value)
  }	
}