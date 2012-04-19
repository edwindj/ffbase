#! Rscript

library(devtools)

pkg <-as.package("pkg")
if (check(pkg)) {
  dir.create("output", showWarnings=FALSE)
  build(pkg, "output")
}