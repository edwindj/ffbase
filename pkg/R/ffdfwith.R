#' Evaluate an expression in a ffdf data environment 
#' 
#' Evaluate an R expression in an environment constructed from a ffdata data frame.
#' Faster than \code{\link{with.ffdf}}, but in constrast \code{ffdfwith} can change the original data.
#' Please note that you should write
#' your expression as if it is a normal \code{data.frame}. The resulting return value
#' however will be a \code{ff} object.
#' @export
#'
#' @example ../examples/ffdfwith.R
#' @param data \code{\link{ffdf}} data object used as an environment for evaluation.
#' @param expr expression to evaluate.
#' @param ... arguments to be passed to future methods.
#' @return if expression is a \code{vector} a newly created \code{ff} vector will be returned 
#' otherwise if the expression is a data.frame a newly created \code{ffdf} object will be returned.
ffdfwith <- function(data, expr, ...){
   
   es <- deparse(substitute(expr))
   e <- chunkexpr(names(data), es, prefix=".x$")
   .x <- data
   
   chunks <- chunk(.x, ...)
   
   .i <- chunks[[1]]
   res <- eval(e)
   
   if (is.character(res)){
     res <- as.factor(res)
   } else if (is.data.frame(res)){
     fc <- sapply(res, function(x) is.factor(x) || is.character(x))
     res[fc] <- lapply(res[fc], as.factor)
   }
   
   if (is.vector(res) || is.factor(res)){
      res <- as.ff(res)
      length(res) <- nrow(data)
      for (.i in chunks[-1]){
        r <- eval(e)
        if (is.factor(res)){
             r <- as.factor(r)
             levels(res) <- appendLevels(res, levels(r))
         }
         res[.i] <- r
      }
   } else if (is.data.frame(res)){
      res <- as.ffdf(res)
      nrow(res) <- nrow(data)
      for (.i in chunks[-1]){
        r <- eval(e)
        if (any(fc)){
           r[fc] <- lapply(which(fc), function(x) {
                r[[x]] <- as.factor(r[[x]])
                levels(res[[x]]) <<- appendLevels(res[[x]], r[[x]])
                r[[x]]
             })
        }
        res[.i,] <- r
      }
   }
   res
}