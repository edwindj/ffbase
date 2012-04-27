#' Basic statistical functions for ff
#' 
#' Basic statistical functions for \code{\link{ff}} vectors and \code{\link{ffdf}} data.frames. 
#' The aim of ffbase is to make working with ff vectors and ffdf data.frame a little bit easier.
#'
#' @section Basic operations:
#' \tabular{ll}{
#'    \code{\link{cut.ff}} \tab cut a \code{ff} vector. \cr
#'    \code{\link{c.ff}} \tab concatenate \code{ff} vectors. \cr
#'    \code{\link{unique.ff}} \tab unique for a \code{ff} vector. \cr
#' }
#' 
#' @section Selections:
#' \tabular{ll}{
#'    \code{\link{subset.ffdf}} \tab subset a \code{ffdf}. \cr
#'    \code{\link{transform.ffdf}} \tab create a new ffdf based on an existing ffdf \cr
#'    \code{\link{with.ffdf}} \tab create a ff vector based on columns of an existing ffdf \cr
#'    \code{\link{within.ffdf}} \tab create a ffdf data.frame based on columns of an existing ffdf \cr
#'    \code{\link{ffwhich}} \tab create a \code{ff} integer vector based on a logical expression \cr
#' }
#' 
#' @section Aggregations:
#' \tabular{ll}{
#'    \code{\link{hist.ff}} \tab Calculate a histogram for \code{ff} vector. \cr
#'    \code{\link{quantile.ff}} \tab Get quantiles for \code{ff} vector. \cr
#'    \code{\link{sum.ff}} \tab sum for a \code{ff} vector. \cr
#'    \code{\link{mean.ff}} \tab (trimmed) mean for a \code{ff} vector. \cr
#'    \code{\link{all.ff}} \tab all for logical \code{ff} vector. \cr
#'    \code{\link{min.ff}} \tab min for \code{ff} vector. \cr
#'    \code{\link{max.ff}} \tab max for \code{ff} vector. \cr
#'    \code{\link{cumsum.ff}} \tab cumsum for \code{ff} vector. \cr
#'    \code{\link{cumprod.ff}} \tab cumprod for \code{ff} vector. \cr
#'    \code{\link{range.ff}} \tab range for \code{ff} vector. \cr
#'    \code{\link{table.ff}} \tab table for \code{ff} vectors. \cr
#'    \code{\link{tabulate.ff}} \tab tabulate for \code{ff} vectors. \cr
#'    \code{\link{ffdfdply}} \tab Split, group and aggregate for \code{ff} vectors. \cr
#' }
#'
#' @section Miscellaneous:
#' \tabular{ll}{
#'    \code{\link{ffordered}} \tab Add a sorted index to a \code{ff} vector.\cr
#'    \code{\link{ffdfsave}} \tab Save a \code{ffdf} in a directory with its containing \code{ff} columns.\cr
#'    \code{\link{ffappend}} \tab Append data to a \code{ff} vector.\cr
#'    \code{\link{ffdfappend}} \tab Append data to a \code{ffdf}.\cr
#' }
#'
#' @example ../examples/pkg.R
#' @name ffbase-package 
#' @aliases ffbase ffbase-package
#' @docType package 
{}
