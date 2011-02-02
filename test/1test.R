library(ff)

# load sources
src <- sapply(list.files("../pkg/R", full.names=TRUE), source)

# load demos
demos <- sapply(list.files("../pkg/demo", full.names=TRUE), source)

# load examples
examples <- sapply(list.files("../pkg/examples", full.names=TRUE), source)

x <- 1:10
fx <- ff(x)

cut(x, breaks=2)
cut(fx, breaks=2)