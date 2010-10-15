#setwd("/home/marie/Documents/cranvas/")
source("Marie/qscatter.r")

require(RColorBrewer)
require(plumbr)


iris.col = brewer.pal(3, "Set1")[as.integer(iris$Species)]
qiris = qmutaframe(iris, .brushed = FALSE, .color = iris.col)

#set_brush_attr(qiris, '.brushed.size', 2)

display <- qscatter(data = qiris, form = Petal.Length~Sepal.Width)
print(display)
