library(methods)
library(utils)
library(roxygen2)
options(error=traceback)
unlink( 'pkg/man', TRUE)

setwd('pkg')
roxygenize('.', clean = TRUE)

if (length(list.files('inst/doc')) == 0){
   unlink( 'inst/doc', TRUE)   
}
