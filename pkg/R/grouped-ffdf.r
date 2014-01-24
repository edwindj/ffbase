#' A grouped ffdf.
#'
#' The easiest way to create a grouped ffdf is to call the \code{group_by}
#' method on a ffdf or data source: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @param data a data source or data frame.
#' @param vars a list of quoted variables.
grouped_ffdf <- function(data, vars) {
  stopifnot(is.ffdf(data))
  if (length(vars) == 0) return(tbl_ffdf(data))
  
  is_name <- vapply(vars, is.name, logical(1))
  if (!all(is_name)) {
    stop("Data tables can only be grouped by variables, not expressions",
      call. = FALSE)
  }
  structure(data, vars=vars, class = c("grouped_ffdf", "tbl_ffdf", "tbl", class(data)))
}

#' @rdname grouped_ffdf
#' @param x an object to check
#' @export 
is.grouped_ffdf <- function(x) inherits(x, "grouped_ffdf")

#' @export
print.grouped_ffdf <- function(x, ...) {
  cat("Source: local ffdf ", dim_desc(x), "\n", sep = "")
  cat("Groups: ", commas(deparse_all(x)), "\n", sep = "")
  cat("\n")
  trunc_mat(x)
}

#' @export group_size.grouped_ffdf
group_size.grouped_ffdf <- function(x) {
  summarise(x, n = n())$n
}

#' @export regroup.ffdf
regroup.ffdf <- function(x, value) {
  grouped_ffdf(x, unname(value))
}

#' @export regroup.grouped_ffdf 
regroup.tbl_ffdf <- function(x, value) {
  grouped_ffdf(x, unname(value))
}

#' @export regroup.grouped_ffdf 
regroup.grouped_ffdf <- function(x, value) {
  grouped_ffdf(x, unname(value))
}

#' @export ungroup.grouped_ffdf
ungroup.grouped_ffdf <- function(x) {
  tbl_ffdf(x)
}
