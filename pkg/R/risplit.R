#' split using ri
#'
#' @rdname risplit
#' @keywords internal
risplit <- function(x, f){
   UseMethod("risplit")
}

#' @rdname risplit
risplit.default <- function(x, f){
  if (is.factor(f)){
     
     o <- order(f)
     x <- x[o]
     f <- f[o]
     mi <- length(f)
     
     d <- diff(as.integer(f))
     w <- which(d != 0)
     
     from <- c(1,w+1)
     names(from) <- f[from]

     to <- c(w, length(f))
          
     ret <-mapply( function(f,t) {
                     ri(f,t,mi)
                   }
                 , from
                 , to
                 , SIMPLIFY=FALSE
                 )
  }
  structure( ret
           , order=o
           , class="risplit"
           )
}

#' @method risplit ff
#' 
#' @rdname risplit
#' @keywords internal
#' @param f \code{ff factor} object used for splitting
risplit.ff <- function(x, f){
  if (is.factor(f)){
     
     o <- fforder(f)
     f <- f[o]
     mi <- length(f)
     
     start <- 1
     names(start) <- f[start]
     ret <- NULL
     for (i in chunk(f)){
        cf <- f[i]
        d <- diff(as.integer(cf))
        w <- which(d != 0)
        names(w) <- cf[w+1]
        #print(w)
        
        if ((last <- length(w)) == 0){
            next
        }
        
        w <- w + min(i)
        from <- c(start,w[-last])
        to <- w - 1
        ret <-c( ret
               , mapply( function(f,t) {
                           ri(f,t,mi)
                         }
                       , from
                       , to
                       , SIMPLIFY=FALSE
                       )
               )
         start <- w[last]
      }
      ret[[names(start)]] <- ri(start, mi,mi)
  }
  
  structure( ret
           , order=o
           , class="risplit"
           )
}

#' @method risplit ffdf
#' @rdname risplit
risplit.ffdf <- function(x, f){
  stop("Not implemented")
}