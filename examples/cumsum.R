x <- 1:10000
tmp <- cumsum.ff(ff(x), BATCHBYTES=10000)
class(tmp)
table(tmp[] == cumsum(x))

x <- rnorm(1000)
tmp <- cummax.ff(ff(x), BATCHBYTES=10000)
table(tmp[] == cummax(x))
tmp <- cummin.ff(ff(x), BATCHBYTES=10000)
table(tmp[] == cummin(x))
tmp <- cumprod.ff(ff(x), BATCHBYTES=10000)
table(tmp[] == cumprod(x))



