library(roxygen)

roxygenize( 'pkg'
          , roxygen.dir='pkg'
		  , copy.package=FALSE
		  , unlink.target=TRUE
		  , use.Rd2 = TRUE
		  )

unlink( 'pkg/inst', TRUE)