x <- ff(rnorm(10))

# adds an index to x (note the assignment)
x <- addfforder(x)

# retrieve ffindex
o <- ffordered(x)

o
# use it to sort the original vector
x[o]
