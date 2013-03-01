
n <- ffrandom(1E3, rnorm, mean = 10, sd = 5)

set.seed(123)
runif(1)
a <- runif(10)
set.seed(123)
b <- ffrandom(10, runif)
identical(a, b[])

