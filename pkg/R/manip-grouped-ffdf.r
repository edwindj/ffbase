#' Data manipulation for grouped data tables.
#'
#' @param .data 
#' @param ... variables interpreted in the context of \code{.data}
#' @param inplace if \code{FALSE} (the default) the data frame will be copied
#'   prior to modification to avoid changes propagating via reference.
#' @examples
#' if (require("dplyr")) {
#' iris_ffdf <- as.ffdf(iris)
#' species <- group_by(iris_ffdf, Species)
#
#' filter(species, Petal.Width = max(Petal.Width))
#' summarise(players, Petal.Width = mean(Petal.Width))
#' mutate(species, cSepal.Width = Sepal.Width - min(Sepal.Width))
#' arrange(species, Species, desc(Sepal.Width))
#' 
#' }
#' @name manip_grouped_ffdf
NULL

#' @rdname manip_grouped_ffdf
#' @export
filter.grouped_ffdf <- function(.data, ...) {
  expr <- and_expr(dots(...))
  stop("Not implemented")
  grouped_ffdf(
  )
}

#' @rdname manip_grouped_ffdf
#' @export summarise.grouped_ffdf
summarise.grouped_ffdf <- function(.data, ...){
  # TODO check is .data$vars match current index
  
  cols <- named_dots(...)
  stop("Not implemented")
  grouped_ffdf(
    data = out,
    vars = .data$vars
  )
}

#' @rdname manip_grouped_ffdf
#' @export mutate.grouped_ffdf
mutate.grouped_ffdf <- function(.data, ..., inplace = FALSE) {
  data <- .data$obj
  keys <- deparse_all(.data$vars)
  if (!inplace) data <- clone(data)
  stop("Not implemented")
  grouped_ffdf(
    data = data,
    vars = .data$vars
  )
}

#' @rdname manip_grouped_ffdf
#' @export arrange.grouped_ffdf
arrange.grouped_ffdf <- function(.data, ...) {
  stop("Not implemented")
  grouped_ffdf(
    data = out,
    vars = .data$vars
  )
}

#' @rdname manip_grouped_ffdf
#' @export select.grouped_ffdf
select.grouped_ffdf <- function(.data, ...) {
  stop("Not implemented")
  grouped_ffdf(
    data = out,
    vars = .data$vars
  )
}


#' @export do.grouped_ffdf
do.grouped_ffdf <- function(.data, .f, ...) {
  stop("Not implemented")
  eval(call, env)$out
}
