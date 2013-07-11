##
## Rstudio logs
##
input <- list()
input$path <- getwd()
input$path <- "/home/janw/Desktop/ffbaseusage"
input$start <- as.Date('2012-10-01')
input$today <- as.Date('2013-06-10')
input$today <- Sys.Date()-1
input$all_days <- seq(input$start, input$today, by = 'day')
input$all_days <- seq(input$start, input$today, by = 'day')
input$urls <- paste0('http://cran-logs.rstudio.com/', 
                     as.POSIXlt(input$all_days)$year + 1900, '/', input$all_days, '.csv.gz')
##
## Download
##
sapply(input$urls, FUN=function(x, path) {
  print(x)
  try(download.file(x, destfile = file.path(path, strsplit(x, "/")[[1]][[5]])))
}, path=input$path)

##
## Import the data in a csv and put it in 1 ffdf
##
require(ffbase)
files <- sort(list.files(input$path, pattern = ".csv.gz$"))
rstudiologs <- NULL
for(file in files){
  print(file)
  con <- gzfile(file.path(input$path, file))
  x <- read.csv(con, header=TRUE, colClasses = c("Date","character","integer", rep("factor", 6), "numeric"))
  x$time <- as.POSIXct(strptime(sprintf("%s %s", x$date, x$time), "%Y-%m-%d %H:%M:%S"))
  rstudiologs <- rbind(rstudiologs, as.ffdf(x))
}
dim(rstudiologs)
rstudiologs <- subset(rstudiologs, as.Date(time) >= as.Date("2012-12-31"))
ffsave(rstudiologs, file = file.path(input$path, "rstudiologs"))
