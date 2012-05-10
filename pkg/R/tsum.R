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
   
   res <- binned_sum(x, bin, nbins)
   colnms <- colnames(res)
   dim(res) <- c(tdim,2)
   dimnames(res) <- c(lapply(index, levels))
   res
}

# x <- 1:12
# f <- factor(sample(c("a", "b"), 12, replace=TRUE))
# f2 <- factor(sample(c("c", "d"), 12, replace=TRUE))
# 
# tsum(x, list(a=f,f2=f2))
