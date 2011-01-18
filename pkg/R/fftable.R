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
    stop("Not implemented")
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
    bin <- 0L
    lens <- NULL
    dims <- integer(0L)
    pd <- 1L
    dn <- NULL
    for (a in args) {
        if (is.null(lens)) 
            lens <- length(a)
        else if (length(a) != lens) 
            stop("all arguments must have the same length")
        cat <- if (is.factor(a)) {
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
                    exclude = if (useNA == "no") 
                      NA)
                }
            }
        }
        else {
            a <- factor(a, exclude = exclude)
            if (useNA != "no") 
                addNA(a, ifany = (useNA == "ifany"))
            else a
        }
        nl <- length(ll <- levels(cat))
        dims <- c(dims, nl)
        dn <- c(dn, list(ll))
        bin <- bin + pd * (as.integer(cat) - 1L)
        pd <- pd * nl
    }
    names(dn) <- dnn
    bin <- bin[!is.na(bin)]
    if (length(bin)) 
        bin <- bin + 1L
    y <- array(tabulate(bin, pd), dims, dimnames = dn)
    class(y) <- "table"
    y
}