# tapply.ff for sum
tsum <- function(x, index, ...){
   
   if (!is.list(index)){
     index <- list(index)
   }
   
   ndim <- length(index)
   tdim <- sapply(index, nlevels)
   
   ai <- do.call(cbind, index)
   dim(ai) <- c(nrow(ai), ndim)
   
   nbins <- prod(tdim)
   
   bin <- arrayIndex2vectorIndex(ai, tdim)
   
   bs <- binned_sum(x, bin, nbins)
   
   res <- bs[,2]
   # TODO add NA's
   dim(res) <- c(tdim)
   dimnames(res) <- c(lapply(index, levels))
   res
}

# x <- 1:12
# f <- factor(sample(c("a", "b"), 12, replace=TRUE))
# f2 <- factor(sample(c("c", "d"), 12, replace=TRUE))
# 
# tsum(x, list(f=f,f2=f2))
