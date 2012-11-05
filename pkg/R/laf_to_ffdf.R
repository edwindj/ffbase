#' Use LaF to import data into \code{ffdf} data.frame
#'
#' Use \code{LaF} to import data into a \code{\link{ffdf}} data.frame
#' @param laf laf object pointing to a csv or fwf file
#' @param x optional, \code{\link{ffdf}} object where laf data should be appended to.
#' @param nrows, number of rows per block, passed on to \code{next_block} 
#' @param ... passed on to \code{next_block}
#' @export
laf_to_ffdf <- function(laf, x=NULL, nrows=1e5, ...){
  if (!require(LaF)){
    stop("This function needs the package 'LaF', which can be installed from CRAN")
  }
  begin(laf)
  while(nrow(block <- next_block(laf, nrows=nrows, ...)))
    #TODO test if adding columns separately is faster/or that allocating the ff vectors first is faster
    x <- ffdfappend(x, block)
  x
}


# quick testing

# require(LaF)
# generate data set of 1 million observations
# data(iris)
# idx <- sample(nrow(iris), size=1e6, replace=TRUE)
# iris <- iris[idx,]
# 
# tmp <- "d:/temp/test.csv" #tempfile()
# write.table(iris, tmp, row.names=FALSE, col.names=FALSE, sep=",")
# rm(iris)
# 
# col_types <- sapply(iris, storage.mode)
# col_types["Species"] <- "categorical"
# col_names <- names(iris)
# 
# laf <- laf_open_csv(tmp, column_types=col_types, column_names=col_names)
# 
# data <- laf_to_ffdf(laf, nrows=1e6)
# data
# 
# 
# for (n in c(1e3, 1e4, 1e5, 1e6)){
#   cat(n, ":")
#   cat(system.time(laf_to_ffdf(laf, nrows=n)))
# }