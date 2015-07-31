[![Build Status](https://travis-ci.org/edwindj/ffbase.png?branch=master)](https://travis-ci.org/edwindj/f
fbase)
![version](http://www.r-pkg.org/badges/version/ffbase)
![downloads](http://cranlogs.r-pkg.org/badges/ffbase)

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

See also my presentation on ffbase on [UseR 2013](http://www.slideshare.net/EdwindeJonge1/ffbase)

**Working on dplyr for ff in [ffbase2](https://github.com/edwindj/ffbase2)**
