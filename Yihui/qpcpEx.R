## examples of qparallel()

## color palette
library(RColorBrewer)

source('Yihui/qparallel.R')

## old iris...
qparallel(iris, col = brewer.pal(3, "Set1")[as.integer(iris$Species)])
qparallel(iris, scale = "I")
qparallel(iris, scale = "var")
# try other standardizing methods
st2 = function(x) ((x - min(x)) / (max(x) - min(x)))^2
qparallel(iris, scale = "st2")

## formula interface
qparallel(iris, ~ Sepal.Length + Sepal.Width)
# . means all variables in the data frame as usual
qparallel(iris, ~ .)

## vertical
qparallel(iris, col=brewer.pal(3, "Set1")[as.integer(iris$Species)], horizontal = FALSE, mar=c(0.1,.15,0.1,0.05))
formula
qparallel(mtcars)

## test speed
qparallel(matrix(rnorm(1000 * 10), ncol = 10), col = rgb(1, 0, 0, 0.2), mar = c(.2,.1,.1,.1))
qparallel(matrix(runif(10000 * 10), ncol = 10), col = rgb(1, 0, 0, 0.2), scale="var", mar = c(.2,.1,.1,.1))


## residential data: 18221x8
library(YaleToolkit)
data(NewHavenResidential)
qparallel(NewHavenResidential, col = rgb(1, 0, 0, 0.1))

qparallel(NewHavenResidential, col = brewer.pal(3,"Set1")[as.integer(NewHavenResidential$zone)])

qparallel(NewHavenResidential, col = rgb(1, 0, 0, 0.1), horizontal = FALSE)


## Tengfei's Data
chrom2=read.csv('~/Downloads/chrom2.csv')
qparallel(chrom2, mar=c(.1,.1,.05,.1))
qparallel(chrom2, col=rgb(1,0,0,.2), mar=c(.1,.1,.05,.1))
qparallel(chrom2, horizontal=FALSE,mar=c(.1,.1,.05,.1))

ld=read.csv('~/Downloads/ld.csv')
qparallel(ld, col=rgb(0,1,0,.2), mar=c(.1,.1,.05,.1))



## speed for segments and lines? R loops?...

library(qtpaint)

n=1e5; p=10
test=matrix(runif(n*p),ncol=p)
col=rainbow(n)
qtest1 = function(item, painter, exposed) {
print(system.time({
        for (i in 1:n) {
            qdrawLine(painter, 1:p, test[i, ], stroke = col[i])
        }
}))
}
qtest2 = function(item, painter, exposed) {
print(system.time({
        for (i in 1:n) {
	    qdrawSegment(painter, 1:(p-1), test[i, 1:(p-1)], 2:p, test[i, 2:p], stroke = col[i])
        }
}))
}

if (exists('scene')) rm(scene)
scene=qscene()
qlayer(scene, qtest1, limits = qrect(c(1,p),c(0,1)))
qplotView(scene = scene)

if (exists('scene')) rm(scene)
scene=qscene()
qlayer(scene, qtest2, limits = qrect(c(1,p),c(0,1)))
qplotView(scene = scene)


print(system.time({
        for (i in 1:n) {
	    1:(p-1); test[i, 1:(p-1)]; 2:p; test[i, 2:p]; col[i]
        }
}))

# git pull cranvas master


# 'vars' handles formula more accurately: only takes the rhs
# 'scale' accepts custom standardizing functions
# better draw axes with layout() functionalities, but unclear how qtpaint works
# speed up drawing lines?


# draw by group
