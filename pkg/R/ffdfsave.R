# experimental do not use
ffdfsave <- function( x, path, move=TRUE){
   path <- file
   
   #make subdir for all columns
   for (col in names(x)){
      #filename(x[col]) <- ""
   }
   save(x, file=path)
}