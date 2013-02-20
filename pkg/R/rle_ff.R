rle.ff <- function(x, ...){
  rles <- lapply(chunk(x, ...), function(i){
    do.call(cbind, rle(x[i]))
  })
  
  prev_i <- 1
  for (i in seq_along(rles)[-1]){
    r <- rles[[i]]
    r_prev <- rles[[prev_i]]
    N <- nrow(r_prev)
    
    if (r[1,2] == r_prev[N, 2]){
      r_prev[N, 1] <- r_prev[N, 1] + r[1,1]
      r <- r[-1,]
    }
    rles2 <-
  }
  
  #TODO remove duplicate rows
  rles <- do.call(rbind, rles)
  values <- rles[,2]
  lengths <- rles[,1]
  print(which(diff(values) == 0))
  structure( list(lengths=lengths, values=values)
           , class="rle"
           )
}

require(ff)
x <- sample(3, 100, replace=TRUE)
fx <- ff(x)
rle.ff(fx, by=100)
rle.ff(fx, by=10) -> l
l