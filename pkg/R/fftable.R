#' table.ff uses the cross-classifying factors to build a contingency table of the 
#' counts at each combination of factor levels.
#'
#' @seealso \code{\link{table}}
#' @export
#'
#' @param ... \code{ff} factors
#' @param exclude see \code{\link{table}}
#' @param useNA see \code{\link{table}}
#' @param deparse.level see \code{\link{table}}
#'
#' @return \code{table} object
table.ff <- function (...
                    , exclude = if (useNA == "no") c(NA, NaN)
                    , useNA = c("no","ifany", "always")
                    , dnn = list.names(...)
                    , deparse.level = 1
                    ){
   #borrowed from table
   list.names <- function(...) {
      l <- as.list(substitute(list(...)))[-1L]
      nm <- names(l)
      fixup <- if (is.null(nm)) 
         seq_along(l)
      else nm == ""
      dep <- sapply(l[fixup], function(x) switch(deparse.level + 
         1, "", if (is.symbol(x)) as.character(x) else "", 
         deparse(x, nlines = 1)[1L]))
      if (is.null(nm)) 
         dep
      else {
         nm[fixup] <- dep
         nm
      }
   }
   ###
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

setGeneric("table"
          , signature="..."
          )
          
setMethod("table"
         , "ff"
         , table.ff
         )

setMethod("table"
         , "ff_vector"
         , table.ff
         )
