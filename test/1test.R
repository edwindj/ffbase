library(ff)

# load sources
src <- sapply(list.files("../pkg/R", full.names=TRUE), source)

# load demos
#demos <- sapply(list.files("../pkg/demo", full.names=TRUE), source)

# load examples
examples <- sapply(list.files("../examples", full.names=TRUE), source)

x <- ff(as.integer(1:10))
z <- compact(x)
vmode(z)