setwd("/Users/dicook/cranvas/code")
load(".RData")

library(qtpaint)
library(plumbr)
library(RColorBrewer)

# options(verbose = TRUE)
# Yihui
source("Yihui/qparallel.R")
source("utilities/interaction.R")
source("utilities/optimization.R")


## old iris...
#  create mutaframes inside the data first
iris.col = brewer.pal(3, "Set1")[as.integer(iris$Species)]
qiris = qmutaframe(iris, .brushed = FALSE, .color = iris.col)

set_brush_attr(qiris, '.brushed.size', 1)

qparallel(qiris)
qparallel(qiris)

# Hadley's tour
source("Hadley/tourr-gui.r") 
gui_xy(olive)

library(qtbase)
library(qtpaint)
library(ggplot2, warn.conflicts = FALSE)
library(tourr, warn.conflicts = FALSE)
olive$region <- factor(olive$region)
library(colorspace)
library(RGtk2)
library(gWidgets)
library(plumbr)
source("utilities/interaction.R")
source("Di/tourr-gui.r")

qolive <- qmutaframe(olive, .brushed=FALSE)
gui_xy(qolive)

qolive$.brushed[1:300]<-TRUE
  
# Mosaic plots
source("Heike/mosaic-hilite.r")
source("Heike/labels.r")
source("utilities/api-sketch.r")
source("utilities/helper.r")
source("utilities/axes.r")
source("utilities/interaction.R")

qmosaic(qiris, ~Species,"hbar")

set_brush_attr(qiris, '.brushed.color', "orange")

# Tengfei's code
source('Tengfei/eos/R/qcircle-utils.R')
source('Tengfei/eos/R/qcircle-painter.R')
options(stringsAsFactors=FALSE)

