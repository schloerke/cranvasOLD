setwd("/Users/dicook/cranvas/code/Di")
load(".Rdata")

library(qtpaint)
library(plumbr)
library(RColorBrewer)

# options(verbose = TRUE)
# Yihui
source("../utilities/optimization.R")
source("../utilities/interaction.R")
source("../utilities/data.R")
source("../Yihui/qparallel.R")

## old iris...
#  create mutaframes inside the data first
iris.col = brewer.pal(3, "Set1")[as.integer(iris$Species)]
qiris = qmutaframe(iris, .brushed = FALSE, .color = iris.col)
 
set_brush_attr(qiris, '.brushed.size', 1)
 
qparallel(qiris)
qparallel(qiris)

# Hadley's tour
source("../Hadley/tourr-gui.r") 
# gui_xy(olive)

# Di's modifications
library(qtbase)
library(qtpaint)
library(ggplot2, warn.conflicts = FALSE)
library(tourr, warn.conflicts = FALSE)
olive$region <- factor(olive$region)
library(colorspace)
library(RGtk2)
library(gWidgets)
library(plumbr)
library(RColorBrewer)
source("../utilities/interaction.R")
source("tourr-gui.r")

olive.col <- brewer.pal(3, "Set1")[as.integer(olive$region)]
qolive <- qmutaframe(olive, .brushed = FALSE, .color = olive.col)
qparallel(qolive)
gui_xy(qolive)

qolive$.brushed[1:300]<-TRUE
  
# Mosaic plots
source("../Heike/mosaic-hilite.r")
source("../Heike/labels.r")
source("../utilities/api-sketch.r")
source("../utilities/helper.r")
source("../utilities/axes.r")
source("../utilities/interaction.R")

qmosaic(qiris, ~Species,"hbar")

set_brush_attr(qiris, '.brushed.color', "orange")

# Tengfei's code
source(../'Tengfei/eos/R/qcircle-utils.R')
source('../Tengfei/eos/R/qcircle-painter.R')
options(stringsAsFactors=FALSE)

# NRC data
nrcstat = read.csv('../Yihui/nrcstat.csv')
qnrc = qmutaframe(nrcstat)
rownames(qnrc)=paste(nrcstat$Institution.Name, nrcstat$Program.Name, sep = ' -> ')
nms = names(nrcstat)

## Overview: type, rankings
qparallel(qnrc, vars = nms[10:13], main = 'Overview of Rankings', horizontal=FALSE)


set_brush_attr(qnrc, '.label.show', TRUE)
set_brush_attr(qnrc, '.label.color', 'black')

#set_brush_attr(qnrc, '.label.show', FALSE)

qnrc$.color = 'red'

qparallel(qnrc, vars = nms[14:19], main = 'Research, Student Support, Diversity', horizontal=FALSE)

qparallel(qnrc, vars = nms[10:13], main = 'Overview of Rankings', horizontal=FALSE)
qparallel(qnrc, vars = nms[20:68], main = 'Criteria', horizontal=FALSE, center=median, scale="I")
qparallel(qnrc, vars = nms[20:68], main = 'Criteria', horizontal=FALSE, center=median)
qparallel(qnrc, vars = nms[20:68], main = 'Criteria', horizontal=FALSE, scale="I")
