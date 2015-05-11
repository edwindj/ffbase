## extractors
x <- ff(1:10)
y <- ff(11:20)
idx <- ff(c(FALSE, TRUE, NA, TRUE))
x[idx]
idx <- ff(c(FALSE, FALSE, TRUE))
x[idx]
idx <- ff(1:3)
x[idx]

## setters
idx <- ff(c(FALSE, TRUE, NA, TRUE))
x[idx] <- y[idx]
x
idx <- ff(c(FALSE, FALSE, TRUE))
try(x[idx] <- y[idx], silent = T) ## not allowed
x
idx <- ff(1:3)
x[idx] <- y[idx]
x



