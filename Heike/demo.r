library(qtpaint)
library(plumbr)
library(productplots)

# iris data: 
# link parallel coordinate plot and barchart

setwd("../Yihui")
source("qparallel.R")

## color palette
library(RColorBrewer)

## old iris...
#  create mutaframes inside the data first

iris.col = brewer.pal(3, "Set1")[as.integer(iris$Species)]
qiris = qmutaframe(iris, .brushed = FALSE, .color = iris.col)

set_brush_attr(qiris, '.brushed.size', 2)

qparallel(qiris)

setwd("../Heike")
source("mosaic-hilite.r")
qmosaic(qiris, ~X1.Species,"hbar")

set_brush_attr(qiris, '.brushed.color', "orange")

###################################

# mosaics on their own


qhappy <- qmutaframe(happy, .brushed = FALSE)
set_brush_attr(qhappy, '.brushed.color', "yellow")


#plot1 <- qmosaic(happy, ~ health+sex+happy, c("vspine","hspine","hspine"))  
#print(plot1)
plot1 <- qmosaic(happy, ~ happy, c("hbar"))  
plot2 <- qmosaic(happy, ~ degree+sex+happy, c("vspine","hspine","hspine"))  
print(plot1)
#print(plot2)
#happym <- mutaframe(happy)
#qmosaic(happym, ~ health+sex+happy, c("vspine","hspine","hspine"))  

#qmosaic(mutaframe(happy), ~ health+sex+happy, c("vspine","hspine","hspine"))  

#tc <- as.data.frame(Titanic)
#plot1 <- qmosaic(tc, Freq~Survived+Sex+Class+Age, c("vspine","hspine","hspine","hspine"))
#print(plot1)