unique.ff <- function(x, incomparables = FALSE, fromLast = FALSE, ...){
   z <- NULL
   for (i in chunk(x)){
      z <- unique(c(z,x[i]), incomparables, fromLast, ...)
   }
   z
}

#' fftable uses the cross-classifying factors to build a contingency table of the 
#' counts at each combination of factor levels.
#'
#' @export
fftable <- function (...
                    , exclude = if (useNA == "no") c(NA, NaN)
					, useNA = c("no","ifany", "always")
					#, dnn = list.names(...)
					, deparse.level = 1
					){
	
	args <- list(...)
	tab <- NULL
	for (i in chunk(args[[1]])){
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
	   names(dimnames(tab)) <- names(dimnames(ttab))
	}
	return(tab)	
}