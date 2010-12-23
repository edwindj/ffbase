# fffunctions Edwin de Jonge

library(ff)
 
mean.ff <- function(x, trim=0, ...){

    if (trim > 0){
	   trim <- min(trim, 0.5)
	   x <- ffsort(x)
	   n <- length(x)
	   lo <- floor(n * trim)
	   hi <- n - lo
	   if (hi == lo) { 
		lo <- lo -1
		hi <- hi + 1
	   }
	   vw(x) <- c(lo, hi-lo, n - hi)
	   return(mean(x, trim=0, ...))
	}
	
   
   res <- sapply( chunk(x, from=1, along.with=x)
                , function(i){
				     c( mean=mean(x[i], ...)
					  , w = sum(i)/max(i)
					  )
                  }
				)
   weighted.mean(res['mean',], res['w',])
 }

#other implementation of mean, prepared for "foreach"
mean2 <- function(x, ...){
	.combine <- function(x, y){
	  z <- rbind(x,y)
	  mu <- z[,"mu"]
	  n <- z[,"n"]
      
	  c( mu = sum(mu*(n/sum(n)))
	   , n  = sum(n))
	}
	
	.final <- function(x) {as.numeric(x[1])}
	
   acc <- NULL
   
   for (i in chunk(x)){
	  acc <- .combine(acc, c(mu=mean(x[i]), n=sum(i)))
   }
   .final(acc)
}

#' @method sum ff
#' @export
sum.ff <- function(x, ...){
   res <- ffvecapply(  sum(x[i1:i2], ...)
					 , X=x
					 , RETURN = TRUE
					 , CFUN = "csum"
#					 , BATCHSIZE = length/2
#					 , VERBOSE = TRUE
					 )
	res
}

max.ff <- function(x, ...){
	max(sapply( chunk(x)
	          , function(i){
					max(x[i], ...)
	            }
	          )
	   , ...
	   )
}

#' test 1, 2, 3
#' @title BRange
#' @method range ff
#' @export
range.ff <- function(x, ...){
   range(sapply(chunk(x)
               , function(i){
			        range(x[i],...)
			     }
			   )
		, ...
		)
}

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

subset.ff <- function(x, subset, ...){
	#y <- ff(length=sum(subset), vmode=vmode(x))
	y <- clone(x)
	length(y) <- sum(subset, na.rm=TRUE)
	y[] <- x[subset]
	y
}

subset.ffdf <- function(x, subset, ...){
	y <- clone(x)
	n <- 0
	#subset <- eval(expression(substitute(subset)))
	#print(subset)
	#TODO check if subset is an expression or logical vector. If so, then idx vector
	for (i in chunk(x)){
	   dat <- x[i,]
	   row.names(dat) <- min(i):max(i)
	   sel <- eval(substitute(subset), dat, parent.frame())
	   dat <- dat[sel,]
	   s <- nrow(dat) 
	   if (s > 0){
	      y[(n+1):(n+s),] <- dat
		  n <- n + s
	   }
	}
	nrow(y) <- n
	y 
}