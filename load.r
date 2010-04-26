#library(DescribeDisplay)
library(ggplot2)

FILE <- (function() {
  attr(body(sys.function()), "srcfile")
})()$filename
PATH <- dirname(FILE)



#lapply(dir(file.path(PATH, "./"), full.name=T), source)
source("./api-sketch_021910.r")

lapply(dir(file.path(PATH, "Barret"), full.name=T), source)
#lapply(dir(file.path(PATH, "Marie"), full.name=T), source)
lapply(dir(file.path(PATH, "Yihui"), full.name=T), source)


