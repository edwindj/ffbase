#' @export
risplit <- function(x, f){
   UseMethod("risplit")
}

#' @method risplit ff
#' @export
risplit.default <- function(x, f){
  stop("Not implemented")
}

#' @method risplit ff
#' @export
risplit.ff <- function(x, f){
  stop("Not implemented")
}

#' @method risplit ffdf
#' @export
risplit.ffdf <- function(x, f){
  stop("Not implemented")
}