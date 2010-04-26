## examples of qparallel()

## rescale to 0~1
minmax = function(x) {
    xna = x[!is.na(x)]
    (x - min(xna))/(max(xna) - min(xna))
}

## old iris...
iris.sd = apply(sapply(iris, as.numeric), 2, minmax)
qparallel(iris.sd, col = c("red", "blue", "yellow")[as.integer(iris$Species)])

## vertical
qparallel(iris.sd, col = c("red", "blue", "yellow")[as.integer(iris$Species)], 
    horizontal = FALSE)

## test speed
qparallel(matrix(rnorm(10000 * 10), ncol = 10), col = rgb(1, 
    0, 0, 0.1))

## residential data: 18221x8
library(YaleToolkit)
data(NewHavenResidential)
nhr = sapply(NewHavenResidential, as.numeric)
nhr.sd = apply(nhr, 2, minmax)
qparallel(nhr.sd, col = c(rgb(1, 0, 0, 0.1), rgb(1, 
    1, 0, 0.1), rgb(0, 0, 1, 0.1))[nhr[, "zone"]], mar = rep(0.05, 
    4))

#qparallel(nhr.sd, col = c(rgb(1, 0, 0, 0.1), rgb(0, 0, 1, 0.1))[nhr[,
#    'acType']], horizontal = FALSE)
