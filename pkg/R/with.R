#' Evaluate an expression in a ffdf data environment 
#' 
#' Evaluate an R expression in an environment constructed from a ffdf data frame.
#' (see \code{\link{with}}).
#' 
#' @note \code{with.ffdf} may not be the most efficient way for computing the desired
#' result since it does not \code{\link{chunk}} the \code{\link{ffdf}} object. Use
#' in that case \code{\link{ffdfwith}}, \code{\link{within.ffdf}}.
#'  
#' @seealso \code{\link{ffdfwith}}, \code{\link{within.ffdf}}
#' @method with ffdf 
#' @export
#'
#' @example ../examples/with.R
#' @param data \code{\link{ffdf}} data object used as an environment for evaluation.
#' @param expr expression to evaluate.
#' @param ... arguments to be passed to \code{\link{chunk}}.
#' @return if expression is a \code{vector} a newly created \code{ff} vector will be returned 
#' otherwise if the expression is a data.frame a newly created \code{ffdf} object will be returned.
with.ffdf <- function(data, expr, ...){
  eval(substitute(expr), physical(data), enclos=parent.frame())
}

#' Evaluate an expression in a ffdf data environment 
#' 
#' Same functionality as \code{\link{within}}. Please note that you should write
#' your expression as if it is a normal \code{data.frame}. The resulting data.frame
#' however will be a new \code{ffdf} data.frame.
#' @method within ffdf 
#' @export
#'
#' @example ../examples/within.R
#' @param data \code{\link{ffdf}} data object used as an environment for evaluation.
#' @param expr expression to evaluate.
#' @param ... arguments to be passed to \code{\link{chunk}}.
#' @return a modified clone of \code{data}.
within.ffdf <- function(data, expr, ...){
    expr <- substitute(expr)
    parent <- parent.frame()
    
    #chunks <- chunk(data, by=2) debug chunking
    chunks <- chunk(data, ...)
    cdat <- data[chunks[[1]],,drop=FALSE]
   
    e <- evalq(environment(), cdat, parent)
    eval(expr, e)
    l <- as.list(e)
    
    l <- l[!sapply(l, is.null)]
    del <- setdiff(names(cdat), names(l))
    #delete 
    cdat[del] <- list()
    cdat[names(l)] <- l

    res <- as.ffdf(cdat)
    rownames(res) <- NULL
    nrow(res) <- nrow(data)
    rownames(res) <- rownames(data)
    for (i in chunks[-1]){
       cdat <- data[i,,drop=FALSE]
       e <- evalq(environment(), cdat, parent)
       eval(expr, e)
       l <- as.list(e)
       l <- l[!sapply(l, is.null)]
       cdat[names(l)] <- l
       cdat[del] <- list()
       res[i,] <- cdat
    }
    res
}
