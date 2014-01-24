#' Create a ffdf source.
#'
#' A ffdf source wraps a local ffdf.
#'
#' @export
#' @param data a ffdf data.frame
#' @examples
#' if (require("ffbase")) {
#' ds <- tbl_ffdf(mtcars)
#' as.ffdf(ds)
#' as.tbl(mtcars)
#' ds
#' }
tbl_ffdf <- function(data) {
  if (!require("dplyr")) {
    stop("dplyr package required to use tbl_ffdf", call. = FALSE)
  }
  if (is.grouped_ffdf(data)) return(ungroup(data))
  
  if (!is.ffdf(data)){
    #TODO convert character vector into factors?
    data <- as.ffdf(data)
  }
  
  structure(data, class = c("tbl_ffdf", "tbl", class(data)))
}

#' @export as.tbl.ffdf
as.tbl.ffdf <- function(x, ...) {
  tbl_ffdf(x)
}

#' @export tbl_vars.tbl_ffdf
tbl_vars.tbl_ffdf <- function(x) names(x)

#' @export tbl_vars.ffdf
tbl_vars.ffdf <- function(x) names(x)

#' @export groups.ffdf
groups.ffdf <- function(x) { NULL}

#' @export ungroup.tbl_ffdf
ungroup.tbl_ffdf <- function(x) x

#' @export ungroup.ffdf
ungroup.ffdf <- function(x) x

#' @export same_src.tbl_ffdf
same_src.tbl_ffdf <- function(x, y){
  ff::is.ffdf(y)
}

# Standard data frame methods --------------------------------------------------

#' @export as.data.frame.tbl_ffdf
as.data.frame.tbl_ffdf <- function(x, row.names = NULL, optional = FALSE, ...) {
  NextMethod()
}

#' @export print.tbl_ffdf
print.tbl_ffdf <- function(x, ...) {
  cat("Source:       ffdf ", dim_desc(x), "\n", sep = "")
  cat("\n")
  trunc_mat(x)
}

#' @export dimnames.tbl_ffdf
dimnames.tbl_ffdf <- function(x) dimnames.ffdf(x)

#' @export dim.tbl_ffdf
dim.tbl_ffdf <- function(x) dim.ffdf(x)

#' @export head.tbl_ffdf
head.tbl_ffdf <- function(x, n=6L, ...) x[seq_len(n), ] # NOTE no negative n supported!

#' @export tail.tbl_ffdf
tail.tbl_ffdf <- function(x, n=6L, ...) x[seq(from=(nrow(x)+1-n), to=nrow(x)),]