#' Create a ffdf source.
#'
#' A ffdf source wraps a local ffdf.
#'
#' @export
#' @param data a ffdf data.frame
#' @examples
#' if (require("ffbase")) {
#' ds <- tbl_ffdf(mtcars)
#' ds
#' }
tbl_ffdf <- function(data) {
  if (!require("ffbase")) {
    stop("ffbase package required to use ffdf", call. = FALSE)
  }
  if (is.grouped_ffdf(data)) return(ungroup(data))
  
  if (!is.ffdf(data)) data <- as.ffdf(data)
  
  structure(data, class = c("tbl_ffdf", "tbl", class(data)))
}

#' @export
as.tbl.ffdf <- function(x, ...) {
  tbl_ffdf(x)
}

#' @export
tbl_vars.tbl_ffdf <- function(x) names(x)

#' @export
tbl_vars.ffdf <- function(x) names(x)

# Standard data frame methods --------------------------------------------------


#' Coerce data table to source.
#'
#' @export
#' @keywords internal
as.ffdf.tbl_ffdf <- function(x, keep.rownames = NULL) {
  if (!is.null(keep.rownames)) {
    warning("keep.rownames argument ignored", call. = FALSE)
  }

  x
}

#' @export
as.data.frame.tbl_ffdf <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
  if (!identical(optional, FALSE)) warning("optional argument ignored", call. = FALSE)
  as.data.frame.ffdf(x, ...)
}

#' @export
print.tbl_ffdf <- function(x, ...) {
  cat("Source:     ffdf ", dim_desc(x), "\n", sep = "")
  cat("\n")
  # TODO add head and tail to ffdf
  trunc_mat(x)
}

#' @export
dimnames.tbl_ffdf <- function(x) dimnames.ffdf(x)

#' @export
dim.tbl_ffdf <- function(x) dim.ffdf(x)

#' @export
head.tbl_ffdf <- function(x, n=6L, ...) x[seq_len(n), ] # NOTE no negative n supported!

#' @export
tail.tbl_ffdf <- function(x, n=6L, ...) tail(x, n=n, ...)