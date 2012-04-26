#' Compact a ff vector or ffdf data frame
#'
#' Compact takes a ff vector and tries to use the smallest binary data type for this vector.
#' @aliases compact compact.ff compact.ffdf
#' @method compact ff
#' @method compact ffdf
#' @export
#' @param x \code{ff} or \code{ffdf} object
#' @param use.na \code{logical} if TRUE the resulting ff vector can contain NA, otherwise not
#' @param ... other parameters
#' @return compact ff vector
#' @keywords internal
compact <- function(x, use.na=TRUE, ...){
   UseMethod("compact")
}

compact.ff <- function(x, use.na=TRUE,...){
   switch( vmode(x)
         , integer = {
              lev <- levels(x)
              levels(x) <- NULL
              r <- range(x, na.rm=TRUE)
              
              if (r[1] <= 0)
                return(x)
              as.ff(x, vmode="byte")
            }
        , x
        )
}

compact.ffdf <- function(x, use.na, ...){
   ret <- lapply(physical(x), compact, use.na)
   do.call(ffdf, ret)
}