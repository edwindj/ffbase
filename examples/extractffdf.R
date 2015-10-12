## extractors for ffdf objects
data(iris)
x <- as.ffdf(iris)
x[x$Sepal.Length > 5, ]
x[x$Sepal.Length > 5, 1:3]
x[x$Sepal.Length > 5, 1, drop=TRUE]
x[x$Sepal.Length > 5, 1]
x[, 1]
x[, ]
x[c("Sepal.Length", "Sepal.Width")]
x[1:2]

## setters
data(iris)
x <- as.ffdf(iris)
testpositions <- x$Sepal.Length > 5
testpositions <- ffwhich(testpositions, testpositions == TRUE)
mynewdata <- x[testpositions, c("Sepal.Length", "Sepal.Width")]
mynewdata$Sepal.Length <- ff(1, length = nrow(mynewdata))
x[x$Sepal.Length > 5, c("Sepal.Length", "Sepal.Width")] <- mynewdata
x[testpositions, ]

data(iris)
x <- as.ffdf(iris)
testpositions <- x$Sepal.Length > 5
testpositions <- ffwhich(testpositions, testpositions == TRUE)
mynewdata <- x[testpositions, c("Sepal.Length", "Sepal.Width")]
mynewdata$Sepal.Length <- ff(1, length = nrow(mynewdata))
x[testpositions, c("Sepal.Length", "Sepal.Width")] <- mynewdata
x[testpositions, ]




