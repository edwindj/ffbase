checkRange <- function(range, x){
    
    if (is.null(range)){
       return(ri(1,length(x)))
    }
     
    #TODO add checks
    range
}

#' Groups the input integer vector into several groups if the running cumulative sum increases a certain maximum number
#'
#' Groups the input integer vector into several groups if the running cumulative sum increases a certain maximum number
#'
#' @useDynLib ffbase
#' @param x an integer vector
#' @param max the maximum running cumulative size before an extra grouping is done
#' @return An integer vector of the same length of x, indicating groups
grouprunningcumsum <- function(x, max){
	l <- as.integer(length(x))
	if(l == 0){
		return(x)
	}
	x <- as.integer(x)	
	max <- as.integer(max)
	result <- .C("grouprunningcumsum",
			x = x, 
			l = l, 
			max = max,
			PACKAGE="ffbase")
	result$x
}

as.ffdf.list <- function(x){
  if(sum(sapply(x, FUN=function(x) !inherits(x, "ff_vector"))) > 0){
    stop("the elements of x need to be ff_vectors")
  }
  if(length(unique(sapply(x, FUN=function(x) length(x)))) != 1){
    stop("the elements of x need to be ff_vectors of the same length")
  }
  measures <- names(x)
  for(i in 1:length(measures)){
    measure <- measures[i]
    if(i == 1){
      result <- ffdf(x[[measure]])
      colnames(result) <- measure  	
      result[[measure]] <- x[[measure]]			
    }else{
      result[[measure]] <- x[[measure]]
    }  			
  }
  result
}  	
  	
coerce_to_allowNA <- function(x){
  recoder <- function (x, from = c(), to = c()){
    missing.levels <- unique(x)
    missing.levels <- missing.levels[!missing.levels %in% from]
    if (length(missing.levels) > 0) {
      from <- append(x = from, values = missing.levels)
      to <- append(x = to, values = missing.levels)
    }
    to[match(x, from)]
  }
  coerceto <- sapply(names(.vimplemented)[.vimplemented==TRUE], FUN=function(x) names(maxffmode(x, vmode(as.ff(NA)))))  
  coerceto <- recoder(x, from = names(coerceto), to = coerceto)
  names(coerceto) <- names(x)
  list(x = x, coerceto = coerceto)
}


ffbaseffdfindexget <- function(x, index, indexorder = NULL, ...){
	os <- ffindexordersize(length=NROW(x), vmode="integer")
	o <- ffindexorder(index, os$b)
	res <- list()
	for(measure in names(x)){
		open(x[[measure]])
		res[[measure]] <- ffindexget(x=x[[measure]], index=index, indexorder=o)
		close(x[[measure]])
	}
	as.ffdf(res)
}
