#' Data manipulation for ffdf.
#'
#' @param .data a ffdf
#' @param ... variables interpreted in the context of \code{.data}
#' @param inplace if \code{FALSE} (the default) the data frame will be copied
#'   prior to modification to avoid changes propagating via reference.
#' @examples
#' if (require(dplyr)){
#'    mtcars <- as.ffdf(mtcars)
#'    filter(mtcars, cyl == 8)
#'    select(mtcars, mpg, cyl, hp:vs)
#'    arrange(mtcars, cyl, disp)
#'    mutate(mtcars, displ_l = disp / 61.0237)
#'    summarise(mtcars, mean(disp))
#'    #summarise(group_by(mtcars, cyl), mean(disp))
#' }
#' @name manip_ffdf
NULL

and_expr <- function(exprs) {
  stopifnot(is.list(exprs))
  
  if (length(exprs) == 0) return(TRUE)
  if (length(exprs) == 1) return(exprs[[1]])

  left <- exprs[[1]]
  for (i in 2:length(exprs)) {
    left <- substitute(left & right, list(left = left, right = exprs[[i]]))
  }
  left
}

#' @rdname manip_ffdf
#' @export filter.ffdf
filter.ffdf <- function(.data, ..., env=parent.frame()) {
  expr <- and_expr(dots(...))
  idx <- ffwhich(.data, as.expression(expr), envir=env)

  rownames(.data) <- NULL
  # this selection only works on ffdf without rownames...
  .data[idx, ]
}

#' @rdname manip_ffdf
#' @export filter.tbl_ffdf
filter.tbl_ffdf <- function(.data, ..., env=parent.frame()) {
  tbl_ffdf(
    filter.ffdf(.data, ..., env=env)
  )
}

#' @rdname manip_ffdf
#' @export summarise.ffdf
summarise.ffdf <- function(.data, ...) {
  cols <- named_dots(...)
  
  data_env <- list2env(physical(.data), parent = parent.frame())
  data_env$count <- function() nrow(.data)
  
  for (col in names(cols)) {
    data_env[[col]] <- as.ff(eval(cols[[col]], data_env))
  }
  
  do.call("ffdf", (mget(names(cols), data_env)))
#   quote
#   l <- list()
#   for (col in names(cols)){
#     a <- substitute(.data$col, list(col=col))
#   }
#   a
}

#' @rdname manip_ffdf
#' @export summarise.tbl_ffdf
summarise.tbl_ffdf <- function(.data, ...) {
  tbl_ffdf(
    summarise.ffdf(.data$obj, ...)
  )
}

#' @rdname manip_ffdf
#' @export mutate.ffdf
mutate.ffdf <- function(.data, ..., inplace = FALSE) {
  if (!inplace) .data <- clone(.data)
  eval(substitute(transform.ffdf(.data, ...)))
}

#' @rdname manip_ffdf
#' @export mutate.tbl_ffdf
mutate.tbl_ffdf <- function(.data, ...) {
  tbl_ffdf(
    mutate.ffdf(.data, ...)
  )
}

#' @rdname manip_ffdf
#' @export arrange.ffdf
arrange.ffdf <- function(.data, ...) {
  vars <- dots(...)
  vars <- sapply(vars, function(v){substitute(.data$v, list(v=v))})
  idx <- eval(substitute(do.call("fforder", vars)))
  row.names(.data) <- NULL
  .data[idx,]
}

#' @rdname manip_ffdf
#' @export arrange.tbl_ffdf
arrange.tbl_ffdf <- function(.data, ...) {
  tbl_ffdf(
    arrange.ffdf(.data, ...)
  )
}

#' @rdname manip_ffdf
#' @export select.ffdf
select.ffdf <- function(.data, ...) {
  input <- var_eval(dots(...), .data, parent.frame())
  select_eval(input, .data)
}

#' @rdname manip_ffdf
#' @export select.tbl_ffdf
select.tbl_ffdf <- function(.data, ...) {
  tbl_ffdf(
    select.ffdf(.data, ...)
  )
}

#' @rdname manip_ffdf
#' @export do.ffdf
do.ffdf <- function(.data, .f, ...) {
  list(.f(as.data.frame(.data), ...))
}

#' @rdname manip_ffdf
#' @export do.tbl_ffdf
do.tbl_ffdf <- function(.data, .f, ...) {
  list(.f(as.data.frame.ffdf(.data), ...))
}
