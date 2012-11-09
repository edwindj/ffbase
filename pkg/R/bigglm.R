#' Bounded memory linear regression
#'
#' bigglm.ffdf creates a generalized linear model object that uses only p^2 memory for p variables.
#' It uses the biglm package and is a simple wrapper to allow to work with an ffdf as input data. 
#' Make sure that package is loaded.
#'
#' @param formula a model formula
#' @param data an object of class ffdf
#' @param family A glm family object
#' @param ... other parameters passed on to bigglm. See the bigglm package: \code{\link[biglm]{bigglm}}
#' @param chunksize Size of chunks for processnig the ffdf
#' @return An object of class bigglm. See the bigglm package for a description: \code{\link[biglm]{bigglm}}
#' @export 
#' @seealso \code{\link[biglm]{bigglm}}
#' @example ../examples/bigglm.R
bigglm.ffdf<-function(formula, data, family = gaussian(), ..., chunksize=5000){
  if (!require(biglm)){
    stop("This function needs the package 'biglm', which can be installed from CRAN")
  }
  terms<-terms(formula)
  modelvars<-all.vars(formula)  
  dots<-as.list(substitute(list(...)))[-1]
  dotvars<-unlist(lapply(dots,all.vars))
  vars<-unique(c(modelvars,dotvars))

  tablevars<-vars[vars %in% colnames(data)]
  chunks <- bit::chunk(1, NROW(data), by = chunksize)
  got<-0
  chunk<-function(reset=FALSE){
    if(reset){
      if(got>0){
        got<<-0
      }
      return(TRUE)
    }
    if ((got+1) > length(chunks)) 
      return(NULL)
    rval<- data[chunks[[got+1]], tablevars, drop=FALSE]
    got<<-got+1
    return(rval)
  }
  rval<-bigglm(formula, data=chunk, family=family, ...)
  rval$call<-sys.call()
  #rval$call[[1]]<-as.name(.Generic)
  rval$call[[1]]<-as.name("bigglm")
  rval
}



##### quick testing code ######
# require(ff)
# require(biglm)
# download.file("http://faculty.washington.edu/tlumley/NO2.dat", "NO2.dat")
# airpoll <- read.table.ffdf(file="NO2.dat")
# colnames(airpoll) <- c("logno2","logcars","temp","windsp","tempgrad","winddir","hour","day")
# dim(airpoll)
# c <- bigglm(exp(logno2) ~ logcars + temp + windsp, data=airpoll, family=Gamma(log), start=c(2,0,0,0), 
#             maxit=100, chunksize=10)
# summary(c)