authors <- data.frame(
    surname = c("Tukey", "Venables", "Tierney", "Ripley", "McNeil"),
    nationality = c("US", "Australia", "US", "UK", "Australia"),
    deceased = c("yes", rep("no", 4)))
books <- data.frame(
    name = c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core"),
    title = c("Exploratory Data Analysis",
              "Modern Applied Statistics ...",
              "LISP-STAT",
              "Spatial Statistics", "Stochastic Simulation",
              "Interactive Data Analysis",
              "An Introduction to R"),
    other.author = c(NA, "Ripley", NA, NA, NA, NA,
                     "Venables & Smith"))
books <- lapply(1:2000, FUN=function(x, books){
	books$price <- rnorm(nrow(books))
	books
}, books=books)
books <- do.call(rbind, books)
authors <- as.ffdf(authors)                
books <- as.ffdf(books)

dim(books)
dim(authors)
## Left outer join
m1 <- merge(books, authors, by.x = "name", by.y = "surname", all.x=TRUE, all.y=FALSE, BATCHBYTES.x = 20000, BATCHBYTES.y = 20000, trace = TRUE)                     
class(m1)
dim(m1)
names(books)
names(m1)
## Inner join
m1 <- merge(books, authors, by.x = "name", by.y = "surname", all.x=FALSE, all.y=FALSE, BATCHBYTES.x = 20000, BATCHBYTES.y = 20000, trace = TRUE)                     
dim(m1)
## Speedup for inner and left outer join
m1 <- merge(books, authors, by.x = "name", by.y = "surname", all.x=FALSE, all.y=FALSE, speedup=TRUE, BATCHBYTES.x = 20000, BATCHBYTES.y = 20000, trace = FALSE)
dim(m1)
m1 <- merge(books, authors, by.x = "name", by.y = "surname", all.x=TRUE, all.y=FALSE, speedup=TRUE, BATCHBYTES.x = 20000, BATCHBYTES.y = 20000, trace = FALSE)
dim(m1)



