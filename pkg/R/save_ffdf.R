#' Save ffdf data.frames in a directory
#'
#' \code{ffdfsave} saves all ffdf data.frames in the given \code{dir}. Each column
#' is stored as with filename <ffdfname><colname>.ff. All variables are stored in .RData in the same directory.
#' will be named "<columnname>.ff".
#' A saved ffdf data.frame is a .rdata file and can be loaded with the \code{load} function
#' @example ../examples/save_ffdf.R
#' @param ... \code{ffdf} data.frames, \code{ff} vectors, or other variables to be saved in the directory
#' @param dir  path where .rdata file will be saved and all columns of supplied \code{ffdf}'s. It will be created if it doesn't exist.
#' @param clone should the data.frame be cloned?
#' @param relativepath \code{logical} if \code{TRUE} the stored ff vectors will have relative paths, making moving the data to another storage a simple
#' copy operation. 
#' @export
save.ffdf <- function(..., dir="./db", clone=FALSE, relativepath=clone){
  
   names <- as.character(substitute(list(...)))[-1L]
   dir.create(dir, showWarnings=FALSE, recursive=TRUE)
   
   oldwd <- setwd(dir)
   on.exit(setwd(oldwd))
   
   for (n in names){
     x = get(n, pos=1)
     if (is.ffdf(x)) {
       if (isTRUE(clone)){
         x <- clone(x)
       }
       assign(n, move.ffdf(x, dir=".", name=n, relativepath=relativepath))
     }
   }
   
   save(list=names, file=".RData")
   
   rp <- file(".Rprofile", "wt")
   writeLines(".First<-", rp)
   writeLines(deparse(first), rp)
   close(rp)
   
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

move.ffdf <- function(x, dir=".", name=as.character(substitute(x)), relativepath=FALSE){  
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

load.ffdf <- function(dir, envir=parent.frame()){
  oldwd <- setwd(dir)
  on.exit(setwd(oldwd))
  
  env <- new.env()
  
  load(".RData", envir=env)
  names <- ls(envir=env, all.names=TRUE)
  
  for (n in names){
    x = get(n, envir=env)
    if (is.ffdf(x)){
      for (i in physical(x)){
        filename(i) <- filename(i)
      }
      close(x)
    } else if (is.ff(x)){
      filename(x) <- filename(x)
      close(x)
    }
    assign(n, x, envir=envir)
  }
  invisible(env)
}

pack.ffdf <- function(file, ...){
  td <- tempfile("pack")
  save.ffdf(..., dir=td, clone=TRUE, relativepath=TRUE)
  
  file.create(file)
  file <- file_path_as_absolute(file)
  file.remove(file)
  
  oldwd <- setwd(td)
  on.exit(setwd(oldwd))
  
  d <- c(".Rprofile", ".RData", dir(td))
  
  # if file extension is zip, zip it otherwise tar.gz it
  switch( file_ext(file)
        , zip = zip(zipfile=file, files=d)
        , tar(tarfile=file, ".", compression="gzip")
        )
}

unpack.ffdf <- function(file, dir=NULL, envir=parent.frame()){
  if (is.null(dir)){ 
    dir <- tempfile("unpack")
  }
  
  switch( file_ext(file)
        , zip = unzip(zipfile=file, exdir=dir)
        , untar(tarfile=file, exdir=dir)
  )
  
  env <- load.ffdf(dir, envir=envir)
  invisible(env)
}

first <- function(){
  
  if (!require(ffbase)){
    stop("Please install package ff, otherwise the files cannot be loaded.")
  }
  
  for (n in ls()){
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

# x <- as.ffdf(iris)
# pack.ffdf("test.zip", x)