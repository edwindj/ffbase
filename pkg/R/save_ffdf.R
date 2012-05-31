#' Save ffdf data.frames in a directory
#'
#' \code{ffdfsave} saves all ffdf data.frames in the given \code{dir}. Each column
#' is stored as with filename <ffdfname><colname>.ff. All variables are stored in .RData in the same directory.
#' will be named "<columnname>.ff".
#' A saved ffdf data.frame is a .rdata file and can be loaded with the \code{load} function
#' @example ../examples/save_ffdf.R
#' @param ... \code{ffdf} data.frames, \code{ff} vectors, or other variables to be saved in the directory
#' @param dir  path where .rdata file will be saved and all columns of supplied \code{ffdf}'s. It will be created if it doesn't exist.
#' @param relativepath \code{logical} if \code{TRUE} the ff vectors will have relative paths, making the copying the directory a simple
#' copy operation. 
#' @export
save.ffdf <- function(..., dir="./db", relativepath=TRUE){
  
   names <- as.character(substitute(list(...)))[-1L]
   dir.create(dir, showWarnings=FALSE, recursive=TRUE)
   
   oldwd <- setwd(dir)
   on.exit(setwd(oldwd))
   
   for (n in names){
     x = get(n)
     if (is.ffdf(x)) {
       assign(n, move.ffdf(x, dir=".", name=n, relativepath=relativepath))
     }
   }
   
   save(list=names, file=".RData")

   if (relativepath){
     for (n in names){
       x = get(n)
       if (is.ffdf(x)){
         for (i in physical(x)){
           filename(i) <- filename(i)
         }
         close(x)
       } else if (is.ff(x)){
          filename(x) <- filename(x)
          close(x)
       }
     }
   }
}

move.ffdf <- function(x, dir="./", name=as.character(substitute(x)), relativepath=FALSE){  
  dir.create(dir, showWarnings=FALSE, recursive=TRUE)
  for (colname in names(x)){
    ffcol <- x[[colname]]
    
    ffcolname <- file.path(dir, paste(name, "$", colname, ".ff", sep=""))
    
    # move file to right directory
    filename(ffcol) <- ffcolname
    
    # set path to relative path, BEWARE if wd is changed to should be reset!
    if (isTRUE(relativepath)){
      physical(ffcol)$filename <- ffcolname
    }
  }
  close(x)
  x
}

# x <- as.ffdf(iris)
# x1 <- move.ffdf(x, dir="./db3")
# 
# save.ffdf(x, dir="./db4")