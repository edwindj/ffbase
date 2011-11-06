#' create an index from filter statement
#' \code{makeffindex} creates an \code{\link{ff}} integer index vector
#' from a filter statement. The resulting vector can be used to index or subset
#' a ffdf or ff vector.
#' @example ../examples/makeffindex.R
#' @seealso ffindexget ffindexset
#' @param x \code{ff} or \code{ffdf} object
#' @param expr R code that evaluates to a logical
#' @param ... not used
#' @export
makeffindex <- function(x, expr, ...){
  UseMethod("makeffindex")
}

#' @method makeffindex ff_vector
#' @export
makeffindex.ff_vector <- function(x, expr, ...){
  #chunkify expression
  es <- deparse(substitute(expr))
  xs <- deparse(substitute(x))
  
  varre <- paste("\\b(",xs,")\\b", sep="")
  es <- gsub(varre, "\\1[i]", es)
  e <- parse(text=es)
  ###
  fltr <- NULL
  for (i in chunk(x)){
    a <- which(eval(e)) +  min(i) - 1L
    if (length(a))
      fltr <- ffappend(fltr, a)
  }
  fltr
}

#' @method makeffindex ffdf
#' @export
makeffindex.ffdf <- function(x, expr, ...){
  #### chunkify expression
  es <- deparse(substitute(expr))
  for (var in names(x)){
    varre <- paste("\\b(",var,")\\b", sep="")
    es <- gsub(varre, "x$\\1[i]", es)
  }
  e <- parse(text=es)
  ####
  
  fltr <- NULL
  for (i in chunk(x)){
    a <- which(eval(e)) +  min(i) - 1L
    if (length(a))
      fltr <- ffappend(fltr, a)
  }
  fltr
}

###### quick testing
z <- ff(10:1)
idx <- makeffindex(z, z < 5)
z[idx][]
# 
# dat <- ffdf(x1=x, y1=x)
# idx <- makeffindex(dat, x1 < 5 & y1 > 2)
# dat[idx,][,]
