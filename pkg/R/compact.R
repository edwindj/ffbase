compact <- function(x, use.na=TRUE, ...){
   UseMethod("compact")
}

compact.ff <- function(x, use.na=TRUE,...){
   switch( vmode(x)
         , integer = {
              lev <- levels(x)
              levels(x) <- NULL
              r <- range(x, na.rm=TRUE)
              
              if (r[1]) <= 0)
                return(x)
              
              print(r)
              z
              as.ff(x, vmode="byte")
            }
        , x
        )
}

compact.ffdf <- function(x, use.na, ...){
   ret <- lapply(physical(x), compact, use.na)
   do.call(ffdf, ret)
}