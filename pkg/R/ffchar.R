# ff fucntions

.charToList <- function(x){
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
	
	fc <- list( from = from
	          , len  = len
			  , raw  = raw
			  )
	fc
}

.appendCharList <- function(x, y){
   if (is.ffchar(y)){
      for (i in chunk(y$from)){
	     x <- .appendCharList(x,y[i])
	  }
   }
   else {
      fc <- .charToList(y)
	  len <- length(x)
	  nlen <- length(y)
	  length(x) <- len + nlen
	  i <- hi(len+1, len+nlen)
	  x$from[i] <- fc$from + len
	  x$len[i] <- fc$len
      x$raw <- ffappend(x$raw, fc$raw)	  
   }
   x
}

#' ff character function
#' 
#' Implementation of character vector in ff
#' @export
#' @return an ffchar object
ffchar <- function(x, ...){
    #TODO check if x is a character vector
    #TODO if x is an ffchar, copy and reorder/compact it
    #TODO deal with NA's
    fc <- lapply(.charToList(x), ff)
	class(fc) <- c("ffchar")
    return(fc)
}

#' Extracting character from ffchar
#'
#' @name Extract
#' @aliases "[.ffchar" "[<-.ffchar"
#' @method "[" ffchar "[<-." ffchar
#' @export "[.ffchar" "[<-.ffchar"
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

#' @method `[<-` ffchar
#' @name AssignffChar
`[<-.ffchar` <- function(x,i,value){
   stopifnot(is.character(value))
   
   #just append the new character vector at the end...
   nff <- .charToList(value)
   
   len <- length(x$raw)
   nlen <- length(nff$raw)
   length(x$raw) <- len + nlen
   
   x$raw[hi(len+1, len+nlen)] <- nff$raw
   x$len[i] <- nff$len
   x$from[i] <- nff$from + len
   
   x
}

#' Check if objects is an ffchar
#' @export
is.ffchar <- function(x){
   inherits(x, "ffchar")
}

#' Compactify an ffchar function
#'
#' When elements of an ffchar object are changed, the physical object will grow. 
#' @export
compact <- function(x){
   y <- x
   y$len <- clone(x$len)
   y$from <- clone(x$from)
   y
}

#' Length of ffchar
#' 
#' @aliases length length<-
#' @method length ffchar
#' @method length<- ffchar
length.ffchar <- function(x){
   return(length(x$from))
}

`length<-.ffchar` <- function(x, value){
   length(x$from) <- value
   length(x$len) <- value
   x
}

#' @method `print<-` ffchar
print.ffchar <- function(x, ...){
   r <- 1:min(10,length(x))
   s <- x[r]
   names(s) <- paste("[",r,"]", sep="")
   print(s)
}