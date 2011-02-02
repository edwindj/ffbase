#' fftable uses the cross-classifying factors to build a contingency table of the 
#' counts at each combination of factor levels.
#'
#' @seealso \code{\link{table}}
#' @export
#' @param ... \code{ff} factors
#' @param exclude see \code{\link{table}}
#' @param useNA see \code{\link{table}}
#' @param deparse.level see \code{\link{table}}
#' @return \code{table} object
fftable <- function (...
                    , exclude = if (useNA == "no") c(NA, NaN)
                    , useNA = c("no","ifany", "always")
                    , deparse.level = 1
                    ){
	args <- list(...)
	tab <- NULL
   
   dat <- do.call(ffdf, args) # create a ffdf  for estimating good chunking size and checking if ... have equal length
   
	for (i in chunk(dat)){
	   factors <- lapply(args, function(f){
	      f[i]
	   })
	   
	   factors$exclude <- exclude
	   factors$useNA <- useNA
	   factors$deparse.level <- deparse.level
	   
	   ttab <- do.call(table,factors)
	   tab <- if (is.null(tab)){ 
	             ttab
			  }
	          else { tab + ttab
			  }
	   #names(dimnames(tab)) <- names(dimnames(ttab))
	}
	return(tab)	
}