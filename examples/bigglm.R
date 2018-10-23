\dontrun{
  
library(biglm)
library(ff)

data(trees)
x <- as.ffdf(trees)
a <- bigglm(log(Volume)~log(Girth)+log(Height), 
            data=x, chunksize=10, sandwich=TRUE)
summary(a)

b <- bigglm(log(Volume)~log(Girth)+log(Height)+offset(2*log(Girth)+log(Height)),
            data=x, chunksize=10, sandwich=TRUE)
summary(b)

}