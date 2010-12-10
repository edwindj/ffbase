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
	
	cmean <- function(res){
		res <- crbind(res)
		weighted.mean(res[,'mean'], res[,'w'])
	}

   res <- ffvecapply( c( mean=mean(x[i1:i2], ...)
                       , w = (1+(i2-i1))/length(x)
			 	       )
					 , X=x
					 , RETURN = TRUE
					 , CFUN = "list"
#					 , BATCHSIZE = length(x)/2
#					 , VERBOSE = TRUE
					 )
	cmean(res)
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

min.ff <- function(x, ...){
	min(sapply( chunk(x)
	          , function(i){
					min(x[i], ...)
	            }
	          )
	   , ...
	   )
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