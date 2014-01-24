# auxilary functions used in dplyr functions (but may be useful elsewhere)

# copied from dplyr
dots <- function (...) 
{
  eval(substitute(alist(...)))
}

deparseall <- function (x) 
{
  deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), 
                                collapse = "")
  vapply(x, deparse2, FUN.VALUE = character(1))
}

commas <- function (...){
  paste0(..., collapse = ", ")
}