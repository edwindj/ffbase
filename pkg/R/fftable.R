makeFactor <- function(a, exclude, useNA){
	if (is.factor(a)) {
		if (any(is.na(levels(a)))) 
			a
		else {
			if (is.null(exclude) && useNA != "no") 
			  addNA(a, ifany = (useNA == "ifany"))
			else {
			  if (useNA != "no") 
				a <- addNA(a, ifany = (useNA == "ifany"))
			  ll <- levels(a)
			  a <- factor(a, levels = ll[!(ll %in% exclude)], 
				exclude = if (useNA == "no") NA)
			}
		}
    }
    else {
		a <- factor(a, exclude = exclude)
		if (useNA != "no") 
			addNA(a, ifany = (useNA == "ifany"))
		else a
    }	
}

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
					, dnn = list.names(...)
					, deparse.level = 1
					){
    #stop("Not implemented")
    list.names <- function(...) {
	     seq_along(list(...))
 #       l <- as.list(substitute(list(...)))[-1L]
 #       nm <- names(l)
        # fixup <- if (is.null(nm)) 
            # seq_along(l)
        # else nm == ""
        # dep <- sapply(l[fixup], function(x) switch(deparse.level + 
            # 1, "", if (is.symbol(x)) as.character(x) else "", 
            # deparse(x, nlines = 1)[1L]))
        # if (is.null(nm)) 
            # dep
        # else {
            # nm[fixup] <- dep
            # nm
        # }
    }
    if (!missing(exclude) && is.null(exclude)) 
        useNA <- "always"
    useNA <- match.arg(useNA)
    args <- list(...)
    if (length(args) == 0L) 
        stop("nothing to tabulate")
    if (length(args) == 1L && is.list(args[[1L]])) {
        args <- args[[1L]]
        if (length(dnn) != length(args)) 
            dnn <- if (!is.null(argn <- names(args))) 
                argn
            else paste(dnn[1L], seq_along(args), sep = ".")
    }
    bin <- ff(0)
    lens <- NULL
    dims <- integer(0L)
    pd <- 1L
    dn <- NULL
	print(length(args))
    for (a in args) {
	    print(a)
        if (is.null(lens)){
            lens <- length(a)
			length(bin) <- lens
	    }
        else if (length(a) != lens) 
            stop("all arguments must have the same length")
		ll <- if (is.factor) {
		            levels(a)
			  }
			  else {
			     sort(unique(a))
			  }
			  
		for (i in chunk(bin)){
		   cat <- factor(a[i], levels=ll, exclude=exclude)
		   bin[i, add=TRUE] <- pd * (as.integer(cat) - 1)
		}
        nl <- length(ll)
        dims <- c(dims, nl)
        dn <- c(dn, list(ll))
        pd <- pd * nl
	}
	names(dn) <- dnn
    if (length(bin)) 
        for (i in chunk(bin)){
		   bin[i, add=TRUE] <- 1
		}
    y <- fftabulate(bin, pd)
	dims(y) <- dims
	dimnames(y) <- dn
    y
}