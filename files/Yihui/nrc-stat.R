library(plumbr)
library(qtpaint)

## nrcstat = read.csv('../data/nrcstat.csv')
## source('../load.r')
data(nrcstat)
## names(nrc)[8]='Regional.Code'
## nrc$Regional.Code=factor(nrc$Regional.Code, labels=c('NE','MW','SA','SC', 'W'))

qnrc = qmutaframe(nrcstat)
rownames(qnrc)=paste(nrcstat$Institution.Name, nrcstat$Program.Name, sep = ' -> ')

## Overview: type, rankings
qparallel(qnrc, vars = 10:13, main = 'Overview of Rankings')
qnrc$.color = 'red'
qparallel(qnrc, vars = 10:13, main = 'Overview of Rankings', glyph='tick', center=median, boxplot=TRUE, order = TRUE)

## How to find out ISU by intersection and negation? public, midwest, large program

## show data labels
brush_attr(qnrc, '.label.show') = TRUE
brush_attr(qnrc, '.label.color') = 'yellow'

brush_attr(qnrc, '.label.show') = FALSE

qnrc$.color = 'red'

qparallel(qnrc, vars = 14:19, main = 'Research, Student Support, Diversity')

qparallel(qnrc, vars = 20:26, main = 'Publication, Award, Time to Degree')


