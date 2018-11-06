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
books <- lapply(1:100, FUN=function(x, books){
	books$price <- rnorm(nrow(books))
	books
}, books=books)
books <- do.call(rbind, books)
authors <- as.ffdf(authors)                
books <- as.ffdf(books)

dim(books)
dim(authors)
## Inner join
oldffbatchbytes <- getOption("ffbatchbytes")
options(ffbatchbytes = 100)
m1 <- merge( books, authors, by.x = "name", by.y = "surname"
           , all.x=FALSE, all.y=FALSE, trace = TRUE)
dim(m1)
unique(paste(m1$name[], m1$nationality[]))
unique(paste(m1$name[], m1$deceased[]))
m2 <- merge( books[,], authors[,], by.x = "name", by.y = "surname"
           , all.x=FALSE, all.y=FALSE, sort = FALSE)
dim(m2)
unique(paste(m2$name[], m2$nationality[]))
unique(paste(m2$name[], m2$deceased[]))
## Left outer join
m1 <- merge( books, authors, by.x = "name", by.y = "surname"
           , all.x=TRUE, all.y=FALSE, trace = TRUE)
class(m1)
dim(m1)
names(books)
names(m1)
unique(paste(m1$name[], m1$nationality[]))
unique(paste(m1$name[], m1$deceased[]))

authors$test <- ff(TRUE, length=nrow(authors), vmode = "logical")
m1 <- merge( books, authors, by.x = "name", by.y = "surname"
           , all.x=TRUE, all.y=FALSE, trace = TRUE)
vmode(m1$test)
table(m1$test[], exclude=c())
options(ffbatchbytes = oldffbatchbytes)
