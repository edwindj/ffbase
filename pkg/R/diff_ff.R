
diff.ff <- function(x, ...){
  #TODO generalize for lag and difference
  chunks <- chunk(x, by=2)
  
  # hmm, next line may go wrong for small datatypes...
  d <- ff(vmode=vmode(x), length=length(x)-1)
  for (i in chunks){
    j <- i
    d[i] <- diff(x[i])
  }
  d
}