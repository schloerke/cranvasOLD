## examples of qparallel()

## color palette
library(RColorBrewer)

source('qparellel.R')

## old iris...
qparallel(iris, col = brewer.pal(3,"Set1")[as.integer(iris$Species)])

qparallel(iris, ~Sepal.Length+Sepal.Width)

## vertical
qparallel(iris, col=brewer.pal(3,"Set1")[as.integer(iris$Species)], 
    horizontal = FALSE, mar=c(0.1,.15,0.1,0))

## test speed
qparallel(matrix(runif(1000 * 10), ncol = 10), col = rgb(1, 
    0, 0, 0.1), mar = c(.2,.1,.1,.1))

## residential data: 18221x8
library(YaleToolkit)
data(NewHavenResidential)
qparallel(NewHavenResidential, col = rgb(1, 0, 0, 0.1))

#qparallel(nhr.sd, col = c(rgb(1, 0, 0, 0.1), rgb(0, 0, 1, 0.1))[nhr[,
#    'acType']], horizontal = FALSE)


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

rm(scene)
scene=qscene()
qlayer(scene, qtest1, limits = qrect(c(1,p),c(0,1)))
qplotView(scene = scene)

rm(scene)
scene=qscene()
qlayer(scene, qtest2, limits = qrect(c(1,p),c(0,1)))
qplotView(scene = scene)


print(system.time({
        for (i in 1:n) {
	    1:(p-1); test[i, 1:(p-1)]; 2:p; test[i, 2:p]; col[i]
        }
}))
