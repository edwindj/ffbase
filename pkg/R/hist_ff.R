#' hist for ff vectors
#'
#' @method hist ff_vector
#' @param x \code{ff} vector
#' @param breaks
#' @plot
hist.ff <- function(x, breaks=max(100, length(x)), plot=TRUE, ...){
  xname <- deparse(substitute(x))
  
  # TODO improve breaks...
  rng <- range(x)
  breaks <- seq(from=rng[1], to=rng[2], length.out=breaks+1)
  nB <- length(breaks)
  n <- length(x)
  
  equidist <- TRUE
  counts <- integer(nB-1)
  
  for (i in chunk(x, ...)){
    fi <- findInterval(x[i], breaks, rightmost.closed=TRUE)
    counts <- counts + tabulate(fi, nbins=nB-1)
  }
  
  dens <- counts/(n * diff(breaks))
  mids <- 0.5 * (breaks[-1L] + breaks[-nB])
  
  r <- structure( list( breaks = breaks
                      , counts = counts
                      , intensities = dens
                      , density = dens
                      , mids = mids
                      , xname = xname
                      , equidist = equidist
                      )
                , class = "histogram"
                )
  if (plot){
    plot(r)
    invisible(r)
  } else {
    r
  }
}

# x <- ff(rnorm(1000000))
# hist.ff(x)