## examples of qparallel()

## hints for interaction: 
## double click to switch between drawing the brush and moving the brush

# options(verbose = TRUE)
source("qparallel.R")

## color palette
library(RColorBrewer)

## old iris...
qparallel(iris, col = brewer.pal(3, "Set1")[as.integer(iris$Species)])
qparallel(iris, scale = "I")
qparallel(iris, scale = "var")
# try other standardizing methods
st2 = function(x) ((x - min(x))/(max(x) - min(x)))^2
qparallel(iris, scale = "st2")

## subsetting
qparallel(iris, vars = c("Sepal.Length", "Sepal.Width", "Species"))
## or formula interface
qparallel(iris, vars = ~Sepal.Length + Sepal.Width)
# . means all variables in the data frame as usual
qparallel(iris, vars = ~.)

## vertical
qparallel(iris, col = brewer.pal(3, "Set1")[as.integer(iris$Species)], horizontal = FALSE, 
    mar = c(0.1, 0.15, 0.1, 0.05))

## jitter
qparallel(iris, jitter = "Species", amount = 0.3)

## with boxplots
qparallel(iris, col = rgb(1, 0, 0, 0.5), boxplot = TRUE)
qparallel(iris, col = rgb(1, 0, 0, 0.5), boxplot = TRUE, horizontal = FALSE, 
    verbose = TRUE)

qparallel(mtcars)

## test speed
qparallel(matrix(rnorm(1000 * 10), ncol = 10), col = rgb(1, 0, 0, 0.2), mar = c(0.2, 
    0.1, 0.1, 0.1))
qparallel(matrix(rnorm(1000 * 15), ncol = 15), col = rgb(1, 0, 0, 0.2), boxplot = TRUE)
qparallel(matrix(rnorm(5000 * 10), ncol = 10), col = rgb(1, 0, 0, 0.2))
# torture Qt!!
qparallel(matrix(rbeta(1e+05 * 10, 5, 30), ncol = 10), col = rgb(1, 0, 0, 0.05), 
    verbose = TRUE)


## residential data: 18221x8
if (!require("YaleToolkit")) install.packages("YaleToolkit")
library(YaleToolkit)
data(NewHavenResidential)
qparallel(NewHavenResidential, col = rgb(1, 0, 0, 0.1))

# ggplot2
library(ggplot2)
ggpcp(NewHavenResidential) + geom_line()

# lattice
library(lattice)
parallel(NewHavenResidential)

qparallel(NewHavenResidential, col = brewer.pal(3, "Set1")[as.integer(NewHavenResidential$zone)])

qparallel(NewHavenResidential, col = rgb(1, 0, 0, 0.1), horizontal = FALSE)

# jitter is hopeless for huge data...
qparallel(NewHavenResidential, col = rgb(1, 0, 0, 0.01), jitter = "zone", amount = 0.3)
qparallel(NewHavenResidential, col = rgb(1, 0, 0, 0.01), jitter = c("bedrms",'zone'), 
    amount = 0.2)

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

# git pull cranvas master
