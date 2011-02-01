library(roxygen)

unlink( 'pkg/man', TRUE)

roxygenize( 'pkg'
          , roxygen.dir='pkg'
          , copy.package=FALSE
          , unlink.target=TRUE
          , use.Rd2 = TRUE
		    )

unlink( 'pkg/inst/doc', TRUE)