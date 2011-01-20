#' Summary methods for ff objects
#' @export
all.ff <- function(..., na.rm=FALSE){
  y <- list(...)
  for (x in y){
	  for (i in chunk(x)){
	     ret <- all(x[i], na.rm=na.rm)
		 if (is.na(ret) || !ret)
		    return(ret)
	  }
  }
  TRUE
}

#' Summary methods for ff objects
#' @export
any.ff <- function(..., na.rm=FALSE){
  y <- list(...)
  for (x in y){
	  for (i in chunk(x)){
  	     ret <- any(x[i], na.rm=na.rm)
		 if (is.na(ret) || ret)
		    return(ret)
	  }
  }
  FALSE
}