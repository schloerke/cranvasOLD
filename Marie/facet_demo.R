library(qtpaint)

setwd("/Users/marie/")
source("Documents/cranvas/Marie/faceting.R")
source("Documents/cranvas/Marie/api-sketch.r")
source("Documents/cranvas/Marie/draw.R")

#matches ggplot2 example
library(ggplot2)
qplot(mpg,wt,data=mtcars,facets=vs ~gear)

qtfacet(x=1,y=6,data=mtcars,facets=c(8,10))
	
