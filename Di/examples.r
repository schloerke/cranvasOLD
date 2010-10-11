setwd("/Users/dicook/cranvas/code")
load(".RData")

library(qtpaint)
library(plumbr)
library(RColorBrewer)

source("utilities/interaction.R")
source("utilities/optimization.R")

# options(verbose = TRUE)
# Yihui
source("Yihui/qparallel.R")

## old iris...
#  create mutaframes inside the data first
qiris = qmutaframe(iris)

iris.col = brewer.pal(3, "Set1")[as.integer(iris$Species)]

qparallel(qiris, col = iris.col)
qparallel(qiris, col = iris.col)

# Hadley's tour
source("Hadley/tourr-gui.r") 
gui_xy(olive)
