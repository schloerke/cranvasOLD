## examples of qparallel()

## hints for interaction:
## drag with the right button to resize the brush; left button to move the brush

library(qtpaint)
library(plumbr)

# options(verbose = TRUE)
source("qparallel.R")

## color palette
library(RColorBrewer)

## old iris...
#  create mutaframes inside the data first
qiris = qmutaframe(iris)

iris.col = brewer.pal(3, "Set1")[as.integer(iris$Species)]

qparallel(qiris, col = iris.col)
qparallel(qiris, scale = "I")
qparallel(qiris, scale = "var")

# try other standardizing methods
st2 = function(x) ((x - min(x))/(max(x) - min(x)))^2
qparallel(iris, scale = "st2")

## subsetting
qparallel(iris, vars = c("Sepal.Length", "Sepal.Width", "Species"))
## or formula interface
qparallel(iris, vars = ~Sepal.Length + Sepal.Width)
# '.' means all variables in the data frame as usual
qparallel(iris, vars = ~.)

## vertical
qparallel(iris, col = iris.col, horizontal = FALSE)

## jitter
qparallel(iris, jitter = "Species", amount = 0.3)

## with boxplots
qparallel(iris, col = rgb(1, 0, 0, 0.5), boxplot = TRUE)
qparallel(iris, scale = "I", col = rgb(1, 0, 0, 0.5), boxplot = TRUE)
qparallel(iris, col = rgb(1, 0, 0, 0.5), boxplot = TRUE, horizontal = FALSE)

## verbose timing
qparallel(iris, col = rgb(1, 0, 0, 0.5), verbose = TRUE)

## what if there are missing values?
xna = sapply(iris, function(x) {
    x[sample(length(x), 50)] = NA
    x
})
qparallel(xna)

qparallel(mtcars)

## test speed
qparallel(matrix(rnorm(1000 * 10), ncol = 10), col = rgb(1, 0, 0, 0.2),
    mar = c(0.2, 0.1, 0.1, 0.1))
qparallel(matrix(rnorm(1000 * 15), ncol = 15), col = rgb(1, 0, 0, 0.2),
    boxplot = TRUE)
# slow for brushing in my laptop
qparallel(matrix(rnorm(6000 * 10), ncol = 10), col = rgb(1, 0, 0, 0.2),
    verbose = TRUE)
# 1 million segments to torture Qt!!
qparallel(matrix(rbeta(1e+05 * 11, 5, 30), ncol = 11), col = rgb(1, 0,
    0, 0.05), verbose = TRUE)

# linking two parcoords plots: split the data into 2 parts
testdata = as.data.frame(matrix(rnorm(2000 * 10), ncol = 10))
qparallel(testdata, sprintf("V%d", 1:5))
qparallel(testdata, sprintf("V%d", 6:10))


## residential data: 18221x8
if (!require("YaleToolkit")) install.packages("YaleToolkit")
library(YaleToolkit)
data(NewHavenResidential)
qparallel(NewHavenResidential, col = rgb(1, 0, 0, 0.1), verbose = TRUE)

qparallel(NewHavenResidential, vars = names(NewHavenResidential)[1:4], col = rgb(1, 0, 0, 0.1))
qparallel(NewHavenResidential, vars = names(NewHavenResidential)[5:8], col = rgb(1, 0, 0, 0.1))


# ggplot2
library(ggplot2)
ggpcp(NewHavenResidential) + geom_line()

# lattice
library(lattice)
parallel(NewHavenResidential)

qparallel(NewHavenResidential, col = brewer.pal(3, "Set1")[as.integer(NewHavenResidential$zone)])

qparallel(NewHavenResidential, col = rgb(1, 0, 0, 0.1), horizontal = FALSE)

# jitter is hopeless for huge data...
qparallel(NewHavenResidential, col = rgb(1, 0, 0, 0.01), jitter = "zone",
    amount = 0.3)
qparallel(NewHavenResidential, col = rgb(1, 0, 0, 0.01), jitter = c("bedrms",
    "zone"), amount = 0.2)

if (FALSE) {
    ## Tengfei's Data
    chrom2 = read.csv("~/Downloads/chrom2.csv")
    qparallel(chrom2, mar = c(0.1, 0.1, 0.05, 0.1))
    qparallel(chrom2, col = rgb(1, 0, 0, 0.2), mar = c(0.1, 0.1, 0.05, 0.1))
    qparallel(chrom2, horizontal = FALSE, mar = c(0.1, 0.1, 0.05, 0.1))

    ld = read.csv("~/Downloads/ld.csv")
    qparallel(ld, col = rgb(0, 1, 0, 0.2), mar = c(0.1, 0.1, 0.05, 0.1))
}

library(animation)
data(pollen)
qparallel(pollen, col = rgb(0, 0, 1, 0.01))

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
