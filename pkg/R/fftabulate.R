#' \code{fftabulate} takes the integer-valued ff vector bin and counts the number of times each integer occurs in it. 
#'
#' Mimics behaviour of \code{\link{tabulate}}
#' @export
#' @title Tabulation for ff vectors
#' @usage fftabulate(bin, nbins = max(1, bin, na.rm = TRUE))
fftabulate <- function( bin
                      , nbins=max(bin, 1, na.rm=TRUE)
					  , FFRETURN=FALSE
					  ){ 
   #TODO test if bin is integer
   #TODO test if nbins is missing if not, length should be set to max(bin)
   if (is.factor(bin)){
      levels(bin) <- NULL
   }
   if (missing(nbins)){
      maxbins <- nbins
   }
   else {
      maxbins <- max(bin, 1, na.rm=TRUE)
   }
   
   tab <- ff(vmode="integer", length=maxbins)
   for (i in chunk(bin)){
      tab[bin[i], add=TRUE] <- 1
   }   
   length(tab) <- nbins
   
   if (FFRETURN){
      return(tab)
   }
   tab[]
}