#' activate parallel
#'
#' @param cl Cluster to be used for the parallel processing
setCluster <- function(cl=NULL){
  require(parallel)
  setDefaultCluster(cl=cl)
  # TODO set foreach
  e <- parent.env(environment())
  e$forEach <- if (is.null(cl)){
        forEachSEQ
      } else {
        clusterEvalQ(cl=cl, expr=require(ffbase))
        forEachPAR
      }
  invisible()
}