#' create a filter statement resulting in a ff integer vector that can be used for indexing
#' @param x \code{ff} or \code{ffdf} object
#' @param expr R code that evaluates to a logical
#' @param ... not used
#' @export
filter <- function(x, expr, ...){
  UseMethod("filter")
}

#' @method filter ff_vector
filter.ff_vector <- function(x, expr, ...){
  es <- deparse(substitute(expr))
  xs <- deparse(substitute(x))
  
  varre <- paste("\\b(",xs,")\\b", sep="")
  es <- gsub(varre, "\\1[i]", es)
  e <- parse(text=es)
  fltr <- NULL
  for (i in chunk(x)){
    fltr <- ffappend(fltr, which(eval(e)))
  }
  fltr
}

#' @method filter ffdf
filter.ffdf <- function(x, expr, ...){
  es <- deparse(substitute(expr))
  xs <- deparse(substitute(x))
  
  for (var in names(x)){
    varre <- paste("\\b(",var,")\\b", sep="")
    varsub <- paste(xs,"$\\1[i]", sep="")
    es <- gsub(varre, varsub, es)
    #print(list(varre=varre, varsub=varsub, es=es))
  }
  e <- parse(text=es)
  fltr <- NULL
  for (i in chunk(x)){
    fltr <- ffappend(fltr, which(eval(e)))
  }
  fltr
}

###### quick testing
# x <- ff(1:10)
# filter(x, x < 5)
# 
# dat <- ffdf(x1=x, y1=x)
# filter(dat, x1 < 5 & y1 > 2)