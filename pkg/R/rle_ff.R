rle.ff <- function(x, ...){
  rles <- lapply(chunk(x, ...), function(i){
    do.call(cbind, rle(x[i]))
  })
  
  #TODO remove duplicate rows
  rles <- do.call(rbind, rles)
  values <- rles[,2]
  lengths <- rles[,1]
  structure( list(lengths=lengths, values=values)
           , class="rle"
           )
}

# require(ff)
# x <- sample(3, 100, replace=TRUE)
# fx <- ff(x)
# rle.ff(fx, by=100)
# rle.ff(fx, by=10) -> l
# l