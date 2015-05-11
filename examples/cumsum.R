x <- 1:10000
tmp <- cumsum(ff(x))
class(tmp)
table(tmp[] == cumsum(x))

x <- rnorm(1000)
tmp <- cummax(ff(x))
table(tmp[] == cummax(x))
tmp <- cummin(ff(x))
table(tmp[] == cummin(x))
tmp <- cumprod(ff(x))
table(tmp[] == cumprod(x))

## S3 type of calling
cumsum(ff(x))
cummax(ff(x))
cummin(ff(x))
cumprod(ff(x))


