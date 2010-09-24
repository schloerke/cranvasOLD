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
qiris = qmutaframe(iris)

iris.col = brewer.pal(3, "Set1")[as.integer(iris$Species)]

qparallel(qiris, col = iris.col)


setwd("../Heike")
source("mosaic-hilite.r")
qmosaic(qiris, ~Species,"hbar")


###################################

# mosaics on their own


happy <- qmutaframe(happy)

ra <- get_row_attr(happy)
ra$.brushed <- happy$marital =="married"

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