dat <- data.frame(x=1:10, y=10:1)
ffdat <- as.ffdf(dat)

ffdfwith(ffdat, {
   x <- x + 1
   x + y
})

#notice that x has been altered
ffdat$x
