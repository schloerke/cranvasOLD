export LDFLAGS="/home/marie/qtsdk-2010.02/qt/lib"

 export LIBS="/home/marie/qtsdk-2010.02/qt/lib"
 export LD_LIBRARY_PATH=/home/marie/qtsdk-2010.02/qt/lib


library(qtpaint)
library(ggplot2)

#mac path
#setwd("/Users/marie/")
#linux path
setwd("/home/marie/")
source("Documents/cranvas/Marie/faceting.R")
#source("Documents/cranvas/Marie/api-sketch.r")
#source("Documents/cranvas/Marie/draw.R")
source("Documents/cranvas/utilities/helper.r")
source("Documents/cranvas/utilities/axes.r")
source("Documents/cranvas/utilities/api-sketch.r")



#matches ggplot2 examples


qplot(mpg,wt,data=mtcars,facets=vs~gear)
qtfacet(x=1,y=6,data=mtcars,facets=c(10,8))


qplot(mpg,wt,data=mtcars,facets=gear~vs)
qtfacet(x=1,y=6,data=mtcars,facets=c(8,10))
