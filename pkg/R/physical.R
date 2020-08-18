.physical <- function(x){
  .subset2(x, "physical")
}

`.physical<-` <- function (x, value) 
{
  attributes(attr(x, "physical")) <- c(value, list(class = "ff_pointer"))
  x
}