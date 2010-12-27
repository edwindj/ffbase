#'export
risplit <- function(x, f){
   UseMethod("risplit")
}

risplit.default <- function(x, f){
  stop("Not implemented")
}

#' @method risplit ff
risplit.ff <- function(x, f){
  stop("Not implemented")
}

#' @method risplit ffdf
risplit.ffdf <- function(x, f){
  stop("Not implemented")
}