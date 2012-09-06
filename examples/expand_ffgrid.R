comb <- expand.ffgrid(ff(1:1000), ff(factor(LETTERS)))
dim(comb)

x <- ff(factor(LETTERS))
y <- ff(1:1000)
z <- ff(seq.Date(Sys.Date(), Sys.Date()+10, by = "day"))
comb <- expand.ffgrid(x, y, z)
dim(comb)
comb[1:100, ]

