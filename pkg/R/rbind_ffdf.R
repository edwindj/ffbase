#' @S3method rbind ffdf
rbind.ffdf <- function(..., deparse.level=1){
  a <- list(...)
  x <- clone(a[[1]])
  for (l in tail(a, -1)){
    x <- ffdfappend(x, l)
  }
  x
}


#rbind(as.ffdf(iris), as.ffdf(iris))
