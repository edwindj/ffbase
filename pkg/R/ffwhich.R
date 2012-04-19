#' Create an index from a filter statement
#' \code{ffwhich} creates an \code{\link{ff}} integer index vector
#' from a filter expression. The resulting vector can be used to index or subset
#' a ffdf or ff vector.
#' @example ../examples/ffwhich.R
#' @seealso ffindexget ffindexset
#' @param x \code{ff} or \code{ffdf} object
#' @param expr R code that evaluates to a logical
#' @param ... not used
#' @export
ffwhich <- function(x, expr, ...){
  UseMethod("ffwhich")
}

#' @method ffwhich ff_vector
#' @export
ffwhich.ff_vector <- function(x, expr, ...){
  #chunkify expression
  es <- deparse(substitute(expr))
  xs <- deparse(substitute(x))
  .x <- x
  
  varre <- paste("\\b(",xs,")\\b", sep="")
  es <- gsub(varre, ".x[.i]", es)
  e <- parse(text=es)
  ###
  
  fltr <- NULL
  for (.i in chunk(.x, ...)){
    idx  <- which(eval(e))
    fltr <- ffappend(fltr, idx, ...)
  }
  fltr
}

#' @method ffwhich ffdf
#' @export
ffwhich.ffdf <- function(x, expr, ...){
  #### chunkify expression
  es <- deparse(substitute(expr))
  e <- chunkexpr(names(x), es, prefix="x$")
  ####
  
  fltr <- NULL
  for (.i in chunk(x, ...)){
    a <- which(eval(e)) +  min(.i) - 1L
    if (length(a))
      fltr <- ffappend(fltr, a)
  }
  fltr
}

###### quick testing
# x <- ff(10:1)
# idx <- ffwhich(x, x < 5)
# x[idx][]
# 
# dat <- ffdf(x1=x, y1=x)
# idx <- ffwhich(dat, x1 < 5 & y1 > 2)
# dat[idx,][,]