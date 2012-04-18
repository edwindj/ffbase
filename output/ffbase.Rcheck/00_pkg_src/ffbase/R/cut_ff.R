#' cut divides the range of x into intervals and codes the values in x according to which interval they fall. The leftmost interval corresponds to level one, the next leftmost to level two and so on. 
#'
#' The \code{cut} method for ff with the behaviour of \code{link{cut}}
#' @title Convert Numeric ff vector to factor ff
#' @export
#' @seealso cut
#' @method cut ff
#' 
#' @param x a (numeric) ff object that will be cut into pieces
#' @param breaks specifies the breaks for cutting this
#' @param ... other parameters that can be given to \code{\link{cut.default}}
#' 
#' @return ff a new \code{\link{ff}} object with the newly created factor
cut.ff <- function(x, breaks, ...){
   f <- NULL
   
   #### borrowed code from cut.default
	if (length(breaks) == 1) {
		if (is.na(breaks) | breaks < 2) 
			stop("invalid number of intervals")
		nb <- as.integer(breaks + 1)
		dx <- diff(rx <- range(x, na.rm = TRUE))
		if (dx == 0) 
			dx <- abs(rx[1])
		breaks <- seq.int( rx[1] - dx/1000
                       , rx[2L] + dx/1000
                       , length.out = nb
                       )
	}
   ####
   
   args <- list(...)
   
   args$breaks <- breaks
   for (i in chunk(x, ...)){
     args$x <- x[i]
     f <- ffappend( f
                  , do.call(cut, args)
               )
   }
   f
}
