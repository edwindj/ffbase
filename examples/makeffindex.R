# create a ff vector
x <- ff(10:1)
# make an ff index vector
idx <- makeffindex(x, x < 5)
# use it to retrieve values from x
x[idx][]

# create a ffdf data.frame
dat <- ffdf(x1=x, y1=x)
# create an ff index vector from a filter statement
idx <- makeffindex(dat, x1 < 5 & y1 > 2)
# use it to select data from the data.frame
dat[idx,][,]
