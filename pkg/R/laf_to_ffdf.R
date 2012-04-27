#' Use LaF to import data into \code{ffdf} data.frame
#'
#' Use \code{LaF} to import data into a \code{\link{ffdf}} data.frame
#' @param laf laf object pointing to a csv or fwf file
#' @param ... passed on to \code{next_block}
#' @export
laf_to_ffdf <- function(laf, ...){
  if (!require(LaF)){
    stop("This function needs the package 'LaF', which can be installed from CRAN")
  }
  data <- NULL
  while(nrow(block <- next_block(laf, ...)))
    #TODO test if adding columns separately is faster/or that allocating the ff vectors first is faster
    data <- ffdfappend(data, block)
  data
}


# quick testing

# require(LaF)
# # generate data set of 1 million observations
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
# data <- laf_to_ffdf(laf, nrows=1e5)
# data