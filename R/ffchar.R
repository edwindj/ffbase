# fffunctions Edwin de Jonge

library(ff)

DATALENGTH <- 1e4
IDLENGTH <- DATALENGTH/10

xf <- ff(length=DATALENGTH, vmode = "integer")
id <- ff(length=DATALENGTH, vmode = "integer")

ffapply(xf[i1:i2] <- runif(1+i2-i1, max = 10), X = xf, BATCHSIZE = DATALENGTH/2, VERBOSE = TRUE)
ffapply(id[i1:i2] <- runif(1+i2-i1, min=1, max=IDLENGTH+1), X = id, BATCHSIZE = DATALENGTH/2, VERBOSE = TRUE)

#xf
#id[]


#ffapply(summary(id[i1:i2]), X = xf, RETURN=TRUE, CFUN="list", BATCHSIZE = DATALENGTH/2, VERBOSE = TRUE)

clist <- c("Hello","World!")

ffchar <- function(x, ...){
    #TODO check if x is a character vector
    #TODO if x is an ffchar, copy and reorder/compact it
    #TODO deal with NA's
	
    na <- is.na(x)
	if (any(na)){
		x[na] <- ''
	}
	
	lraw <- lapply(x, charToRaw)
	len <- sapply(lraw, length)
	     
	to <- cumsum(len)
	from <- c(1, to[-length(to)] + 1)
	
	from[len==0] <- 1
	is.na(len) <- na
	
	raw <- c(lraw, recursive=TRUE)
	
	fc <- list( from = ff(from)
	          , len  = ff(len)
			  , raw  = ff(raw)
			  )
	
	class(fc) <- c("ffchar")
    return(fc)
}


`[.ffchar` <- function(x,i,...){
   len <- x$len[i]
   
   na <-is.na(len)
   if (any(na)){
		len[na] <- 0
   }
   
   to2 <- cumsum(len)
   from2 <- c(1, to2[-length(to2)] + 1)

   from <- x$from[i]
   to <- from + len - 1
   
   empty <- (len==0)
   if (any(empty)){
     to <- to[!empty]
	 from <- from[!empty]
	 from2[empty] <- to2[empty] <- 1
   }

   #print(list(from2=from2, to2=to2))
   idx <- hi(from,to)
   raw <- x$raw[idx]
   #print(list(len=len, from=from, to=to, raw=rawToChar(raw), na=na, empty=empty, idx=idx))
   
   clist <- mapply( function(f,t){
                        rawToChar(raw[f:t])
                    }
                  , from2
		          , to2
		  )
		  
   if (any(empty)){
      clist[empty] <- ""
   }
   if (any(na)){
	is.na(clist) <- na
   }
   return(clist)
}

length.ffchar <- function(x){
   return(length(x$from))
}

`length<-.ffchar` <- function(x, value){
   length(x$from) <- value
   length(x$len) <- value
   x
}

print.ffchar <- function(x){
   print(x[1:min(10,length(x))])
}

clist <- c("Hello", "", "World!", "", "?")
is.na(clist) <- c(4)

fc <- ffchar(clist)
fc
fc[1]