[![Build Status](https://travis-ci.org/edwindj/ffbase.png?branch=master)](https://travis-ci.org/edwindj/ffbase)

ffbase
======

R is an excellent statistical tool. 
However its important data objects are memory objects: all processing in R takes place in memory.

`ff` is a R package for working with vectors that are bigger than memory, 
but lacks at the moment some standard statistical methods.
The intention of `ffbase` is to provide the basic statistical functions for ff objects, 
so programming with ff will be easier.

To install ffbase from CRAN:

```S
install.packages("ffbase")
```

To install the latest version from github
```
library(devtools)
install_github("edwindj/ffbase", subdir="pkg")
```
