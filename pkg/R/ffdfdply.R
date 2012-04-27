#' Performs a split-apply-combine on an ffdf
#'
#' Performs a split-apply-combine on an ffdf. 
#' Splits the x ffdf according to split and applies FUN to the data, stores the result of the FUN in an ffdf.\cr
#' Remark that this function does not actually split the data. In order to reduce the number of times data is put into RAM for situations with a lot
#' of split levels, the function extracts groups of split elements which can be put into RAM according to BATCHBYTES. Please make sure your FUN covers the
#' fact that several split elements can be in one chunk of data on which FUN is applied.
#'
#' @example ../examples/ffdfplyr.R
#' @param x an ffdf
#' @param split an ff vector which is part of the ffdf x
#' @param FUN the function to apply to each split. This function needs to return a data.frame
#' @param BATCHBYTES integer scalar limiting the number of bytes to be processed in one chunk
#' @param RECORDBYTES optional integer scalar representing the bytes needed to process one row of x
#' @param trace logical indicating to show on which split the function is computing
#' @param ... other parameters passed on to FUN
#' @return 
#' an ffdf
#' @export
#' @seealso \code{\link{grouprunningcumsum}, \link{table.ff}}
ffdfdply <- function(
	x, 
	split, 
	FUN, 
	BATCHBYTES = getOption("ffbatchbytes"), 
	RECORDBYTES = sum(.rambytes[vmode(x)]), 
	trace=TRUE, ...){
	
	if(!is.factor.ff(split)){
		stop("split needs to be an ff factor")
	}
	split.expr <- substitute(split)	
	
	## Detect how it is best to split the ffdf according to the split value -> more than 
	#BATCHBYTES = getOption("ffbatchbytes")
	#RECORDBYTES = sum(.rambytes[vmode(x)])
	MAXSIZE = BATCHBYTES / RECORDBYTES
	splitbytable <- table.ff(eval(split.expr), useNA="ifany")
	splitbytable <- splitbytable[order(splitbytable, decreasing=TRUE)]
	if(max(splitbytable) > MAXSIZE){
		warning("single split does not fit into BATCHBYTES")
	}
	tmpsplit <- grouprunningcumsum(x=as.integer(splitbytable), max=MAXSIZE)
	nrsplits <- max(tmpsplit)
	
	## Loop over the split groups and apply the function
	allresults <- NULL
	for(idx in 1:nrsplits){
		tmp <- names(splitbytable)[tmpsplit == idx]
		if(trace){
			message(sprintf("%s, working on split %s/%s (%s)", Sys.time(), idx, nrsplits, paste(tmp, collapse=", ")))
		}		
		## Filter the ffdf based on the splitby group and apply the function
		fltr <- NULL
		for (i in chunk(x)){
			expr <- parse(text=paste(sprintf("%s[%s]", deparse(split.expr), "i"), " %in% tmp"))
			a <- which(eval(expr)) +  min(i) - 1L
			if (length(a)) fltr <- ffappend(fltr, a)
		}
		inram <- x[fltr, ][, ]
		result <- FUN(inram, ...)	
		rownames(result) <- NULL
		if(!is.null(allresults) & nrow(result) > 0){
			rownames(result) <- (nrow(allresults)+1):(nrow(allresults)+nrow(result))
		}		
		## Push the result to an ffdf
		if(nrow(result) > 0){
			allresults <- ffdfappend(x=allresults, dat=result, recode=FALSE)	
		}				
	}
	allresults
}
