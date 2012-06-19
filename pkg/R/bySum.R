bySum <- function(x, by, na.rm=FALSE, weight=NULL...){
  if (!is.list(by)){
    index <- list(by)
  }
  
  ndim <- length(index)
  tdim <- sapply(index, nlevels)
  
  ai <- do.call(cbind, index)
  dim(ai) <- c(nrow(ai), ndim)
  
  nbins <- prod(tdim)
  
  bin <- arrayIndex2vectorIndex(ai, tdim)
  
  if (missing(weight)){
    weight <- rep(1, length(x))
  }
  stopifnot(length(weight) == length(x))
  
  bs <- .Call("bySum", as.numeric(x), as.integer(bin), as.integer(nbins), as.numeric(weight), PACKAGE = "ffbase")
  
  res <- bs[,2] / bs[,1]
  is.na(res) <- bs[,1] == 0
  
  if (!na.rm){
    is.na(res) <- (bs[,3] > 0)
  }
  
  dim(res) <- c(tdim)
  dimnames(res) <- c(lapply(index, levels))
  res  
}


##### quick testing code ######
# x <- as.numeric(1:100000)
# bin <- as.integer(runif(length(x), 1, 101))
# x[1] <- NA
# 
# bySum(1:10, factor(1:10), weight=10:1)
# 
# system.time({
#   replicate(50, {tapply(x, bin, function(i){c(sum=sum(i, na.rm=TRUE), na=sum(is.na(i)))})})
# })
#    
# system.time({
#   replicate(50, {bySum(x, bin, nbins=100L)})
# })
