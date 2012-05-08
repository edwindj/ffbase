#' Fast tabulating in different bins
#' 
#' \code{binned_sum} implements fast tabulating for given bins by calling c-code. 
#' It also returns the number of NA's per bin.
#' Please note that incorrect use of this function may crash your R-session.
#' the values of \code{bins} must be between \code{1} and \code{nbins} and may not contain \code{NA}.
#' The values of \code{x} must be between \code{1} and \code{nlevels}.
#' @useDynLib ffbase
#' @param x \code{factor} or \code{integer} vector with the data to be tabulated
#' @param bin \code{integer} vector with the bin number for each data point
#' @param nbins \code{integer} maximum bin number 
#' @param nlevels \code{integer} number of levels used in x
#' @return \code{numeric} matrix where each row is a bin and each column a level
#' @export
binned_tabulate <- function (x, bin, nbins=max(bin), nlevels=nlevels(x)){
   stopifnot(length(x)==length(bin))
   
   res <- .Call("binned_tabulate", x, as.integer(bin), as.integer(nbins), as.integer(nlevels), PACKAGE = "ffbase")
   lev <- if (nlevels(x)) c("na", levels(x))
          else c("na", 1:nlevels)
   dimnames(res) <- list(bin = 1:nbins, level=lev)
   res
}

#' @rdname binned_tabulate
#' @usage \method{binned_tabulate}{ff} (x, bin, nbins=max(bin), nlevels=nlevels(x), ...)
#' @param ... passed on to chunk
#' @export
binned_tabulate.ff <- function(x, bin, nbins=max(bin), nlevels=nlevels(x), ...){
  lev <- if (nlevels(x)) c("na", levels(x))
         else c("na", 1:nlevels)
  res <- matrix(0, nrow=nbins, ncol=length(lev), dimnames=list(bin=1:nbins, level=lev))
  for (i in chunk(x, ...)){
    res <- res + .Call("binned_tabulate", x[i], as.integer(bin[i]), as.integer(nbins), as.integer(nlevels), PACKAGE = "ffbase")
  }
  res
}

####### quick test ###################
# size <- 1e5
# x <- sample(c(1:4,NA), size=size, replace=TRUE)
# bin <- sample(1:100, size=size, replace=TRUE)
# nbins <- max(bin, na.rm=TRUE)
# nlevels <- max(x, na.rm=TRUE)
# 
# binned_tabulate(x, bin, nbins, nlevels)
# 
# 
# system.time(
#     replicate( 50
#              , binned_tabulate(x, bin, nbins, nlevels)
#              )
#            )
# 
