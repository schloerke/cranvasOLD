library(plumbr)
library(qtpaint)
source('qparallel.R')

nrcstat = read.csv('nrcstat.csv')
## names(nrc)[8]='Regional.Code'
## nrc$Regional.Code=factor(nrc$Regional.Code, labels=c('NE','MW','SA','SC', 'W'))
nms = names(nrcstat)

qnrc = qmutaframe(nrcstat)
rownames(qnrc)=paste(nrcstat$Institution.Name, nrcstat$Program.Name, sep = ' -> ')

## Overview: type, rankings
qparallel(qnrc, vars = nms[7:13], main = 'Overview of Rankings')
qnrc$.color = 'red'
qparallel(qnrc, vars = nms[7:13], main = 'Overview of Rankings', glyph='tick', center=median, boxplot=TRUE, order = TRUE)

## How to find out ISU by intersection and negation? public, midwest, large program

## show data labels
set_brush_attr(qnrc, '.label.show', TRUE)
set_brush_attr(qnrc, '.label.color', 'yellow')

set_brush_attr(qnrc, '.label.show', FALSE)

qnrc$.color = 'red'

qparallel(qnrc, vars = nms[14:19], main = 'Research, Student Support, Diversity')

qparallel(qnrc, vars = nms[20:26], main = 'Publication, Award, Time to Degree')


