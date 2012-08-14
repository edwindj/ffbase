#' Sequence Generation of \code{ff} vectors.
#'
#' Similar as \code{seq_len} in the base package generates an \code{ff} vector.
#'
#' @export
#' @example ../examples/ffseq.R
#' @param length.out desired length of the sequence. Only non-negative numbers larger than 0 are allowed.
#' @return An ff vector of integers with range from 1 to length.out
#' @seealso \code{\link[base]{seq_len}}
ffseq_len <- function(length.out){
	BATCHBYTES <- getOption("ffbatchbytes")
	length.out <- as.integer(length.out)
	if(length.out < 1){
		stop("length.out needs to be positive")
	}
	bysize <- floor(BATCHBYTES / .rambytes["integer"])
	x <- ff(NA, length=length.out, vmode = "integer")
	for (i in chunk(1, length.out, by=bysize)){
		idx <- as.integer(hi(from=min(i), to=max(i), by = 1L, maxindex = max(i)))
    x[idx] <- idx
  }
	x
}



