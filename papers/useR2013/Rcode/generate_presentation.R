library(knitr)
library(tools)
rnwfile <- "User2013_presentation.Rnw"
output <- "User2013_presentation.pdf"
try(knit2pdf(input = rnwfile))
cleanup <- list.files(pattern = "User2013_presentation[.]")
cleanup <- cleanup[!cleanup %in% c(rnwfile, output)]
file.remove(cleanup)
