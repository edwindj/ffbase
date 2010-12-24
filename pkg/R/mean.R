#' @method mean ff
#' @export
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

#' @export
mean.ffdf <- function(x, ...){
   sapply(physical(x), mean)
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