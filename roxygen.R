library(roxygen)

unlink('pkg/man', TRUE)

setwd("pkg")
roxygenize( '.'
          , roxygen.dir='.'
          , copy.package=FALSE
          , unlink.target=TRUE
          , use.Rd2 = TRUE
		    )

if (length(list.files('inst/doc')) == 0){
   unlink( 'inst/doc', TRUE)   
}
