#' Merge two ffdf by common columns, or do other versions of database join operations. 
#'
#' Merge two ffdf by common columns, or do other versions of database join operations. \cr
#' This method is similar as \code{merge} in the base package with a possible speedup for inner and left outer joins. 
#'
#' @method merge ffdf
#' @export
#' @example ../examples/merge.R
#' @param x an ffdf
#' @param y an ffdf
#' @param by specifications of the common columns. Columns can be specified by name, number or by a logical vector.
#' @param by.x specifications of the common columns of the x ffdf, overruling the by parameter
#' @param by.y specifications of the common columns of the y ffdf, overruling the by parameter
#' @param all see \code{\link{merge}} in R base
#' @param all.x logical passed on to \code{merge}; if TRUE, then extra rows will be added to the output, 
#' one for each row in x that has no matching row in y. 
#' These rows will have NAs in those columns that are usually filled with values from y. The default is FALSE, so that only rows with data from both x and y are included in the output. 
#' @param all.y similar as all.x
#' @param suffixes character(2) specifying the suffixes to be used for making non-by names() unique.
#' @param incomparables values which cannot be matched. See \code{match}.
#' @param speedup logical indicating to use a speedup compared to the regular merge which handles inner and left outer joins. This is
#' done by making a key based on the by.x and by.y columns and matching on the key
#' @param BATCHBYTES.x integer scalar limiting the number of bytes to be processed in one chunk processing the x ffdf
#' @param BATCHBYTES.y integer scalar limiting the number of bytes to be processed in one chunk processing the y ffdf
#' @param RECORDBYTES.x optional integer scalar representing the bytes needed to process one row of x
#' @param RECORDBYTES.y optional integer scalar representing the bytes needed to process one row of y
#' @param trace logical indicating to show on which chunk the function is computing
#' @param ... other parameters passed on to chunk
#' @return 
#' an ffdf
#' @seealso \code{\link{merge}}
merge.ffdf <- function(x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by, all=FALSE, all.x = all, all.y = all, suffixes = c(".x",".y"), 
		incomparables=NULL, speedup=FALSE, 
		BATCHBYTES.x = getOption("ffbatchbytes"), BATCHBYTES.y = getOption("ffbatchbytes"), 
		RECORDBYTES.x = sum(.rambytes[vmode(x)]), RECORDBYTES.y = sum(.rambytes[vmode(y)]),  
		trace=TRUE, ...){
	
	matchmerge <- function(x, y, by.x, by.y, all.x=FALSE, by.iskey=FALSE, suffix = ".y", add.columns=colnames(y), check.duplicates=TRUE, trace=FALSE){
		recoder <- function(x, from=c(), to=c()){
			missing.levels <- unique(x)
			missing.levels <- missing.levels[!missing.levels %in% from]
			if(length(missing.levels) > 0){
				from <- append(x=from, values=missing.levels)
				to <- append(x=to, values=missing.levels)
			}
			to[match(x, from)]
		}
		renameColumns <- function(x, from = "", to = ""){
			if(!"data.frame" %in% class(x)){
				stop("x should be of class data.frame")
			}
			if(length(from) != length(to)) {
				stop("from and to are not of equal length")
			}
			colnames(x) <- recoder(colnames(x), from = from, to = to)
			x
		}
		if(!"data.frame" %in% class(x)){
			stop("x should be of class data.frame")
		}
		if(!"data.frame" %in% class(y)){
			if(!is.vector(y) | by.iskey == FALSE){
				stop("y should be of class data.frame or a vector in which case by.iskey should be TRUE and you should supply by.y as a vector")	
			}else{
				if(length(y) != length(by.y)){
					stop("y is a vector so you have to make sure y and by.y are of the same length")
				}
			}
		}else{
			if(sum(!add.columns %in% colnames(y))){
				stop("all add.columns should be in colnames(y)")
			}	
		}
		
		## Make the keys
		if(by.iskey == FALSE){
			if(trace) message(sprintf("%s: Creating key variables", Sys.time()))
			if(length(by.x) > 1){
				by.lhs.vector <- do.call(paste, as.list(x[, by.x]))	
			}else{
				by.lhs.vector <- x[[by.x]]
			}
			if(length(by.y) > 1){
				by.rhs.vector <- do.call(paste, as.list(y[, by.y]))	
			}else{
				by.rhs.vector <- y[[by.y]]
			}
			#add.columns <- add.columns[!add.columns %in% by.x]
		}else{
			if(length(by.x) != NROW(x)){
				stop("by.x is not of length NROW(x)")
			}
			if(length(by.y) != NROW(y)){
				stop("by.y is not of length NROW(y)")
			}
		}	
		## Remove data from x if inner join
		if(all.x == FALSE){
			if(trace) message(sprintf("%s: Removing left hand side data for inner join", Sys.time()))
			if(by.iskey == FALSE){
				idx <- which(by.lhs.vector %in% by.rhs.vector)
				by.lhs.vector <- by.lhs.vector[idx]
			}else{
				idx <- which(by.x %in% by.y)
			}	
			x <- x[idx, , drop=FALSE]
		}
		## Find the overlapping keys 
		if(trace) message(sprintf("%s: Searching for key matches", Sys.time()))
		if(by.iskey == FALSE){
			if(check.duplicates == TRUE){
				if(sum(duplicated(by.rhs.vector)) > 0){
					stop("Key of y contains doubles which is not allowed for this inner or left outer join")
				}	
			}
			idx <- match(x=by.lhs.vector, table = by.rhs.vector)
		}else{		
			if(check.duplicates == TRUE){
				if(sum(duplicated(by.y)) > 0){
					stop("Key of y contains doubles which is not allowed for this inner or left outer join")
				}	
			}
			idx <- match(x=by.x, table = by.y)
		}
		## Join the data
		if(trace) message(sprintf("%s: Joining the data frames", Sys.time()))
		if("data.frame" %in% class(y)){
			overlapping.columns <- which(add.columns %in% colnames(x))
			if(length(overlapping.columns) > 0){
				result <- cbind(x, renameColumns(
								y[idx, add.columns, drop=FALSE], 
								from = add.columns[overlapping.columns], 
								to = paste(add.columns[overlapping.columns], suffix, sep="")))
			}else{
				result <- cbind(x, y[idx, add.columns, drop=FALSE])
			}
		}else{
			result <- cbind(x, y[idx])
			colnames(result)[ncol(result)] <- add.columns[1]
		}
		
		rownames(result) <- rownames(x)
		result
	}
	
	## Only for speedup
	if(speedup){
		if(all == TRUE || !( (all.x==FALSE & all.y==FALSE) || (all.x==TRUE & all.y==FALSE) ) || !is.null(incomparables)){
			stop("speedup only does inner join and left outer join, incomparables should always be NULL")		
		}
		if(is.numeric(by.x) | is.logical(by.x)){
			by.x <- names(x)[by.x]
		}
		if(is.numeric(by.y) | is.logical(by.y)){
			by.y <- names(x)[by.y]
		}
		if(is.numeric(by) | is.logical(by)){
			by <- names(x)[by]
		}
	}
	## Chunk-wise merging
	allresults <- NULL
	xchunk <- chunk(x, BATCHBYTES=BATCHBYTES.x, RECORDBYTES=RECORDBYTES.x, ...)
	ychunk <- chunk(y, BATCHBYTES=BATCHBYTES.y, RECORDBYTES=RECORDBYTES.y, ...)
	## Depending on which ffdf can be put in RAM most easily, switch the looping in order to avoid importing the dataframes to much again in RAM
	if(trace) message(sprintf("%s, x has %s chunks, y has %s chunks", Sys.time(), length(xchunk), length(ychunk)))
	if(length(xchunk) < length(ychunk)){		
		for (i in xchunk){
			if(trace) message(sprintf("%s, working on x chunk %s:%s", Sys.time(), min(i), max(i)))
			lhs <- x[i,]
			for (j in ychunk){
				if(trace) message(sprintf("%s, working on y chunk %s:%s", Sys.time(), min(j), max(j)))
				rhs <- y[j, ]
				if(speedup){
					result <- matchmerge(x=lhs, y=rhs, by.x=by.x, by.y=by.y, by.iskey=FALSE, suffix = suffixes[2], add.columns=names(y)[!names(y) %in% by.y], check.duplicates=FALSE)
				}else{
					result <- merge(x=lhs, y=rhs, by=by, by.x=by.x, by.y=by.y, all=all, all.x=all.x, all.y=all.y, sort=FALSE, incomparables=incomparables)
				}	
				## Add the result to an ffdf		
				rownames(result) <- NULL
				if(inherits(result, "data.frame") & nrow(result) > 0){
					if(!is.null(allresults)){
						rownames(result) <- (nrow(allresults)+1):(nrow(allresults)+nrow(result))
					}				
					allresults <- ffdfappend(x=allresults, dat=result, recode=FALSE)	
				}		
			}
		}
	}else{
		for (j in ychunk){	
			if(trace) message(sprintf("%s, working on y chunk %s:%s", Sys.time(), min(j), max(j)))
			rhs <- y[j, ]		
			for (i in xchunk){
				if(trace) message(sprintf("%s, working on x chunk %s:%s", Sys.time(), min(i), max(i)))
				lhs <- x[i,]
				if(speedup){
					result <- matchmerge(x=lhs, y=rhs, by.x=by.x, by.y=by.y, by.iskey=FALSE, suffix = suffixes[2], add.columns=names(y)[!names(y) %in% by.y], check.duplicates=FALSE)
				}else{
					result <- merge(x=lhs, y=rhs, by=by, by.x=by.x, by.y=by.y, all=all, all.x=all.x, all.y=all.y, sort=FALSE, incomparables=incomparables)
				}	
				## Add the result to an ffdf		
				rownames(result) <- NULL
				if(inherits(result, "data.frame") & nrow(result) > 0){
					if(!is.null(allresults)){
						rownames(result) <- (nrow(allresults)+1):(nrow(allresults)+nrow(result))
					}				
					allresults <- ffdfappend(x=allresults, dat=result, recode=FALSE)	
				}		
			}
		}
	}	
	allresults
}
