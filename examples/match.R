## Basic example of match.ff
x.ff <- ffmatch(as.ff(as.factor(c(LETTERS, NA))), as.ff(as.factor(c("C","B","Z","X","HMM","Nothing",NA))), trace=TRUE, BATCHBYTES=20)
class(x.ff)
x <- match(c(LETTERS, NA), c("C","B","Z","X","HMM","Nothing",NA))
table(x.ff[] == x, exclude=c())
## %in% is masked from the base package 
as.ff(as.factor(c(LETTERS, NA))) %in% as.ff(as.factor(c("C","B","Z","X","HMM","Nothing",NA)))
as.factor(c(LETTERS, NA)) %in% as.factor(c("C","B","Z","X","HMM","Nothing",NA))


