#' table.ff uses the cross-classifying factors to build a contingency table of the 
#' counts at each combination of factor levels.\cr
#' If \code{...} does not contain factors, \code{unique.ff} will add a levels attribute to the non-factors.
#'
#' Details 
#' @seealso \code{\link{table}}
#' @export
#'
#' @param ... \code{ff} factors or \code{ff} integers
#' @param exclude see \code{\link{table}}
#' @param useNA see \code{\link{table}}
#' @param dnn see \code{\link{table}}
#' @param deparse.level see \code{\link{table}}
#'
#' @return \code{table} object
table.ff <- function( ...
                     , exclude = if (useNA == "no") c(NA, NaN)
                     , useNA = c("no","ifany", "always")
                     , dnn = list.names(...)
                     , deparse.level = 1
){
  ###
  args <- list(...)
  tab <- NULL
  useNA <- match.arg(useNA)
  
  dat <- do.call(ffdf, args) # create a ffdf  for estimating good chunking size and checking if ... have equal length
  colnames(dat) <- names(args)
  ### Cover non-factors like integers by adding a levels attribute
  if(sum(!vmode(dat) %in% c("byte", "short", "integer")) > 0){  	
    stop(sprintf("Only vmodes integer currently allowed - are you sure ... contains only factors or integers?"))
  }
  nonfactors <- sapply(colnames(dat), FUN=function(column, dat) !is.factor.ff(dat[[column]]), dat=dat)
  nonfactors <- names(nonfactors)[nonfactors == TRUE]
  if(length(nonfactors) > 0){
    for(column in nonfactors){
      dat[[column]] <- as.character.ff(dat[[column]])
    } 
  }
  
  for (i in chunk(dat)){
    Log$chunk(i)
    factors <- as.list(dat[i,, drop=FALSE])
    factors$exclude <- exclude
    factors$useNA <- useNA
    factors$deparse.level <- deparse.level
    
    ttab <- do.call(table,factors)
    tab <- if (is.null(tab)){ 
      ttab
    }
    else { tab + ttab
    }
    #names(dimnames(tab)) <- names(dimnames(ttab))
  }
  return(tab)	
}

#borrowed from table
list.names <- function(...) {
  l <- as.list(substitute(list(...)))[-1L]
  nm <- names(l)
  fixup <- if (is.null(nm)) 
    seq_along(l)
  else nm == ""
  
  dep <- sapply(l[fixup], function(x) if (is.symbol(x)) as.character(x) else "")
  if (is.null(nm)) 
    dep
  else {
    nm[fixup] <- dep
    nm
  }
}

# setGeneric( "table"
# , signature="..."
# )

# setMethod( "table"
# , "ff"
# , table.ff
# )

# setMethod( "table"
# , "ff_vector"
# , table.ff
# )
