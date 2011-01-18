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
   if (is.factor(bin)){
      levels(bin) <- NULL
   }
   
   if (missing(nbins)){
      maxbins <- nbins + 1
   }
   else {
      maxbins <- max(bin, 1, na.rm=TRUE) + 1
   }
   
   tab <- if (is.ff(FFRETURN)) {
             FFRETURN
		  }
          else {
		     ff(vmode="integer", length=maxbins)
		  }
		  
   for (i in chunk(bin)){
      tab[na.omit(bin[i]), add=TRUE] <- 1
   }   
   length(tab) <- nbins
   
   if (FFRETURN){
      return(tab)
   }
   tab[]
}