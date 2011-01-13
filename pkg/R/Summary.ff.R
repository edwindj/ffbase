# Summary methods for ff objects

#'@export
all.ff <- function(..., na.rm=FALSE){
  y <- list(...)
  for (x in y){
	  for (i in chunk(x)){
		 if (!all(x[i], na.rm=na.rm)){
			return(FALSE)
		 }
	  }
  }
  TRUE
}

#'@export
any.ff <- function(..., na.rm=FALSE){
  y <- list(...)
  for (x in y){
	  for (i in chunk(x)){
		 if (any(x[i], na.rm=na.rm)){
			return(TRUE)
		 }
	  }
  }
  FALSE
}