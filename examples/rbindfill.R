x <- ffdfrbind.fill( as.ffdf(iris), 
                     as.ffdf(iris[, c("Sepal.Length", "Sepal.Width"
                                     , "Petal.Length")]))
class(x)
nrow(x)
sum(is.na(x$Petal.Width))



