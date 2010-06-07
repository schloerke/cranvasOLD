library(qtpaint)
source("Dropbox/qtinterface/faceting.R")
source("Documents/cranvas/Marie/api-sketch.r")
source("Documents/cranvas/Marie/draw.r")

#matches ggplot2 example
library(ggplot2)
qplot(mpg,wt,data=mtcars,facets=vs ~am)

qtfacet(x=1,y=6,data=mtcars,facets=c(8,9))
	
