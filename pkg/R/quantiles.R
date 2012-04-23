# 
#' quantiles 
#' @method quantile ff
#' @param x \code{ff} vector
#' @param probs ...
#' @param na.rm ... remove na's
#' @param names ...
#' @keywords internal
quantile.ff <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, ...){
  N <- length(x)
  
  nms <- if (names) paste(100*probs, "%", sep="") 
         NULL
  
  qnt <- 1L + as.integer(probs * (N-1))
  #print(qnt)
  
  idx <- ffindex(x)
  if (is.null(idx)) idx <- fforder(x)
  
  ql <- x[idx[qnt]]
  names(ql) <- nms
  ql
}

# x <- ff(1000000:1)
# #x <- addffIndex(x)
# 
# quantile(x)