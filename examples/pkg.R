
ffdat <- as.ffdf(data.frame(x=1:10, y=10:1))

# add a new ff vector z to the ffdf data.frame
within(ffdat, {z <- x+y})[]

# add a new ff vector z to the ffdf data.frame using transform
transform(ffdat, z=x+y)[]

cut(ffdat$x, breaks=3)[]

tabulate.ff(ffdat$x)
