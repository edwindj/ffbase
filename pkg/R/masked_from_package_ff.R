get.ff.default <- get(x="[.ff")
set.ff.default <- get(x="[<-.ff")

"[.ff" <- function(x, i, pack = FALSE){
	if(!missing(i) && is.ff(i) && length(i) > 0 && is.logical(i[1])){
		idx <- ffwhich(i, i==TRUE)
		finalizer(idx) <- "delete"
		get.ff.default(x=x, i=idx, pack=pack)
	}else{
		get.ff.default(x=x, i=i, pack=pack)
	}	
}

"[<-.ff" <- function(x, i, add = FALSE, pack = FALSE, value){
	if(!missing(i) && is.ff(i) && length(i) > 0 && is.logical(i[1])){
			idx <- ffwhich(i, i==TRUE)
			finalizer(idx) <- "delete"
			set.ff.default(x=x, i=idx, add=add, pack=pack, value=value)
	}else{
		set.ff.default(x=x, i=i, add=add, pack=pack, value=value)
	}	
}


