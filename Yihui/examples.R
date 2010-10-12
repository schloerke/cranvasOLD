## examples of qparallel()

## hints for interaction:
## drag with the right button to resize the brush; left button to move the brush

library(qtpaint)
library(plumbr)

## options(verbose = TRUE)
source("qparallel.R")

## color palette
library(RColorBrewer)

## old iris...
##  create a mutaframe containing row attributes first
iris.col = brewer.pal(3, "Set1")[as.integer(iris$Species)]
qiris = qmutaframe(iris, .brushed = FALSE, .color = iris.col)

qparallel(qiris)
qparallel(qiris, scale = "I")
qparallel(qiris, scale = "var")

# try other standardizing methods
st2 = function(x) ((x - min(x))/(max(x) - min(x)))^2
qparallel(qiris, scale = "st2")

## subsetting
qparallel(qiris, vars = c("Sepal.Length", "Sepal.Width", "Species"))
## or formula interface
qparallel(qiris, vars = ~Sepal.Length + Sepal.Width)
# '.' means all variables in the data frame as usual
qparallel(qiris, vars = ~.)

## vertical
qparallel(qiris, horizontal = FALSE)

## jitter
qparallel(qiris, jitter = "Species", amount = 0.3)

## with boxplots
qparallel(qiris, boxplot = TRUE)
qparallel(qiris, scale = "I", boxplot = TRUE)
qparallel(qiris, boxplot = TRUE, horizontal = FALSE)

## set color and print verbose timing
qiris$.color = rgb(1, 0, 0, 0.5)
qparallel(qiris, verbose = TRUE)

## the plot will be updated if we modify the mutaframe
qparallel(qiris)
for (i in 1:30) {
    qiris$Sepal.Length[1] = i
    qiris$.color[1] = sample(colors(), 1)
    Sys.sleep(.5)
}

## what if there are missing values?
xna = qmutaframe(sapply(iris, function(x) {
    x[sample(length(x), 50)] = NA
    x
}))
qparallel(xna)

## centering
qparallel(qiris, scale = 'I', center = median)
## to check we are really centered at the medians
qparallel(qiris, center = median, boxplot = TRUE)

qmtcars = qmutaframe(mtcars)
qparallel(qmtcars)
qparallel(qmtcars, center = median)

## test speed
test.mat1 = qmutaframe(matrix(rnorm(1000 * 10), ncol = 10),
    .color = rgb(1, 0, 0, 0.2))
qparallel(test.mat, mar = c(0.2, 0.1, 0.1, 0.1))

test.mat2 = qmutaframe(matrix(rnorm(1000 * 15), ncol = 15),
    .color = rgb(1, 0, 0, 0.2))
qparallel(test.mat2, boxplot = TRUE)

## slow for brushing in my laptop
test.mat3 = qmutaframe(matrix(rnorm(5000 * 10), ncol = 10),
     .color = rgb(1, 0, 0, 0.05))
qparallel(test.mat3, verbose = TRUE)

## 1 million segments to torture Qt!!
qparallel(qmutaframe(matrix(rbeta(1e+05 * 11, 5, 30), ncol = 11),
                     .color = rgb(1, 0, 0, 0.05)), verbose = TRUE)

# linking two parcoords plots: split the data into 2 parts
testdata = qmutaframe(as.data.frame(matrix(rnorm(2000 * 10), ncol = 10)))
qparallel(testdata, vars = sprintf("V%d", 1:5))
qparallel(testdata, vars = sprintf("V%d", 6:10))


## examples below need to be fixed

## residential data: 18221x8
if (!require("YaleToolkit")) install.packages("YaleToolkit")
library(YaleToolkit)
qnhr = qmutaframe(NewHavenResidential, .color = rgb(1, 0, 0, 0.1))
qparallel(qnhr)

qparallel(qnhr, vars = names(NewHavenResidential)[1:4])
qparallel(qnhr, vars = names(NewHavenResidential)[5:8])


# ggplot2
library(ggplot2)
ggpcp(NewHavenResidential) + geom_line()

# lattice
library(lattice)
parallel(NewHavenResidential)

qnhr$.color = brewer.pal(3, "Set1")[as.integer(NewHavenResidential$zone)]
qparallel(qnhr)

qparallel(qnhr, horizontal = FALSE)

# jitter is hopeless for huge data...
qnhr$.color = rgb(1, 0, 0, 0.01)
qparallel(qnhr, jitter = "zone", amount = 0.3)
qparallel(qnhr, jitter = c("bedrms", "zone"), amount = 0.2)


library(animation)
data(pollen)
qpollen = qmutaframe(pollen, .color = rgb(0, 0, 1, 0.01))
qparallel(qpollen)

## some speed tests; personal use only
if (FALSE) {
    # how fast is the transpose operation?
    p = 10
    n = 1e+06
    y = matrix(runif(n * p), ncol = p)
    x = col(y)

    system.time({
        for (i in 1:10) {
            segx0 = c(t(x[, 1:(p - 1)]))
            segx1 = c(t(x[, 2:p]))
            segy0 = c(t(y[, 1:(p - 1)]))
            segy1 = c(t(y[, 2:p]))
        }
    })
    system.time({
        for (i in 1:10) {
            segx0 = as.vector(t(x[, 1:(p - 1)]))
            segx1 = as.vector(t(x[, 2:p]))
            segy0 = as.vector(t(y[, 1:(p - 1)]))
            segy1 = as.vector(t(y[, 2:p]))
        }
    })
    system.time({
        for (i in 1:10) {
            segx0 = as.vector(t.default(x[, 1:(p - 1)]))
            segx1 = as.vector(t.default(x[, 2:p]))
            segy0 = as.vector(t.default(y[, 1:(p - 1)]))
            segy1 = as.vector(t.default(y[, 2:p]))
        }
    })
    # indexing for large data can be very slow, so pre-process data
    segx0 = as.vector(t.default(x))
    segy0 = as.vector(t.default(y))
    system.time({
        for (i in 1:10) {
            segx0[-p * (1:n)]
            segy0[-p * (1:n)]
            segx0[-(p * (0:(n - 1)) + 1)]
            segy0[-(p * (0:(n - 1)) + 1)]
        }
    })
}

# git pull cranvas master
