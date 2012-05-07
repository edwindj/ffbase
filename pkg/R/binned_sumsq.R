#' Fast summing in different bins
#' 
#' \code{binned_sum} implements fast squared summing for given bins by calling c-code,
#' which can be used to calculate variance and standard deviation
#' Please note that incorrect use of this function may crash your R-session.
#' the values of \code{bins} must be in between 1:\code{nbins} and \code{bin} may not 
#' contain \code{NA}
#' @useDynLib ffbase
#' @param x \code{numeric} vector with the data to be summed squared
#' @param mean \code{numeric} vector with an optional mean to be subtracted from the data to be summed and squared
#' @param bin \code{integer} vector with the bin number for each observation
#' @param nbins \code{integer} maximum bin number 
#' @return \code{numeric} matrix where each row is a bin
#' @export
binned_sumsq <- function (x, mean=rep(0, nbins), bin, nbins=max(bin)){
   stopifnot(length(x)==length(bin))
   stopifnot(length(x)==length(mean))
   res <- .Call("binned_sumsq", as.numeric(x), as.numeric(mean), as.integer(bin), as.integer(nbins), PACKAGE = "ffbase")
   dimnames(res) <- list(bin=1:nbins, c("count", "sumsq"))
   res
}

#' @rdname binned_sumsq
#' @usage \method{binned_sumsq}{ff} (x, bin, nbins=max(bin), ...)
#' @param ... passed on to chunk
#' @export
binned_sumsq.ff <- function(x, bin, nbins=max(bin), ...){
  res <- matrix(0, nrow=nbins, ncol=2, dimnames=list(bin=1:nbins, c("count", "sumsq")))
  for (i in chunk(x, ...)){
    res <- res + .Call("binned_sumsq", as.numeric(x[i]), as.numeric(mean), as.integer(bin[i]), as.integer(nbins), PACKAGE = "ffbase")
  }
  res
}


##### quick testing code ######
# x <- as.numeric(1:100000)
# bin <- as.integer(runif(length(x), 1, 101))
# x[1] <- NA
# 
# binned_sumsq(1:10, rep(1, 10), 1:10, nbins=10)
# binned_sumsq(c(1000,NA), 1:2, 1:2, nbins=2L)
