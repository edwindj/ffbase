library(ff)

# load sources
src <- sapply(list.files("../pkg/R", full.names=TRUE), source)

# load demos
demos <- sapply(list.files("../pkg/demo", full.names=TRUE), source)

b <- factor(rep(c("A","B","C"), 10))
bf <- ff(b)
print(bf)
fftable(bf)
