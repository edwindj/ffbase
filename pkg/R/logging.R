#' logger functions 
#' 
#'

emptyLogger <- function(...) invisible()

Log <- new.env()
Log$info <- cat

#' processing chunk i
#' @param i ri index
Log$chunk <- function(i){
  if (is.na(i[3])){
    Log$info("\rProcessing chunk:",i)    
  } else {
    Log$info("\rProcessing :",round(100*(i[2])/i[3]), "%", sep="")
  }
}

#' sets the logging of ffbase
#' @param level logging level: info/debug
#' @param logger function to be called for logging statements, by default this is \code{cat}
#' @export
set_ffbase_logging <- function(level = c("info"), logger=cat){
  if (!is.function(logger)){
    logger <- emptyLogger
  }
  assign(level, logger, Log)
}
