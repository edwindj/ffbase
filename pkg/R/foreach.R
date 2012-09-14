FOR <- quote(for(a in b) c)
FORPAR <- quote(clusterApply(x=x, fun=fun))
FORMC <- quote(mclapply(X,FUN, mc.cores=1L))
FUNCTION <- quote(function(x) NULL)

forEachSEQ <- function(inexpr, body){
  inexpr <- substitute(inexpr)
  body <- substitute(body)
  
  forloop <- FOR
  forloop[[2]] <- inexpr[[2]]
  forloop[[3]] <- inexpr[[3]]
  forloop[[4]] <- body
  
  eval(forloop, envir=parent.frame())
}

forEachMC <- function(inexpr, body){
  inexpr <- substitute(inexpr)
  body <- substitute(body)

  .i <- deparse(inexpr[[2]])
  
  .fun <- FUNCTION
  names(.fun[[2]]) <- .i
  .fun[[3]] <- body
  
  print(FORMC)
  .formc <- FORMC
  .formc[[2]] <- inexpr[[3]]
  .formc[[3]] <- .fun
  
  print(.formc)
  eval(.formc, envir=parent.frame())
  invisible()
}

forEachPAR <- function(inexpr, body){
  inexpr <- substitute(inexpr)
  body <- substitute(body)
  
  
  .i <- deparse(inexpr[[2]])
  
  .vars <- all.vars(body)
  .vars <- .vars[!.vars %in% .i]
  clusterExport(varlist=.vars, envir=parent.frame())
  
}

forEach <- forEachSEQ

# require(parallel)
# cl <- makeCluster(4)
# setCluster(cl)
# 
# x <- ff(1, length=10)
# forEach(i %in% chunk(x, by=5),{x[i] <- 2})
# close(x)

