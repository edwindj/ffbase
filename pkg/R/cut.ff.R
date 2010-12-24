#' @method cut ff
#' @export
cut.ff <- function(x, breaks, ...){
   f <- NULL
	if (length(breaks) == 1L) {
		if (is.na(breaks) | breaks < 2L) 
			stop("invalid number of intervals")
		nb <- as.integer(breaks + 1)
		dx <- diff(rx <- range(x, na.rm = TRUE))
		if (dx == 0) 
			dx <- abs(rx[1L])
		breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000, 
		length.out = nb)
	}
    else 
		nb <- length(breaks <- sort.int(as.double(breaks)))
    if (anyDuplicated(breaks)) 
        stop("'breaks' are not unique")
    
	codes.only <- FALSE
    if (is.null(labels)) {
        for (dig in dig.lab:max(12, dig.lab)) {
            ch.br <- formatC(breaks, digits = dig, width = 1)
            if (ok <- all(ch.br[-1L] != ch.br[-nb])) 
                break
        }
        labels <- if (ok) 
            paste(if (right) 
                "("
            else "[", ch.br[-nb], ",", ch.br[-1L], if (right) 
                "]"
            else ")", sep = "")
        else paste("Range", seq_len(nb - 1L), sep = "_")
        if (ok && include.lowest) {
            if (right) 
                substr(labels[1L], 1L, 1L) <- "["
            else substring(labels[nb - 1L], nchar(labels[nb - 
                1L], "c")) <- "]"
        }
    }
	   r <- range(x, na.rm=TRUE)
	   for (i in chunk(x)){
		  f <- ffappend( f
		               , ff(cut(x[i], breaks, labels, ...))
					   )
	   }
    f
}
