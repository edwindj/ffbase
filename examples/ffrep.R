ffrep.int(ff(1:1000), times=20)
ffrep.int(ff(factor(LETTERS)), times=20)
ffrep.int(ff(Sys.time()), times=20)
ffrep.int(ff(seq.Date(Sys.Date(), Sys.Date()+10, by = "day")), times=20)

x <- ff(factor(LETTERS), length=26)
ffrep.int(x, times=ff(1:26))

## Or supply an ff vector of the same length as x
x <- seq.Date(Sys.Date(), Sys.Date()+10, by = "day")
x <- as.ff(x)
ffrep.int(x, times=ff(0:10))

x <- ff(factor(LETTERS), length=26)
ffrep.int(x, times=ff(1:26))

