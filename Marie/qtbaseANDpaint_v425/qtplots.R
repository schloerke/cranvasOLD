#qtbase v 0.8-4
#qtpaint v 0.7.9
#svn 425

##########################
###Example###

# not run!
# library(plumbr)
# n <- 10000
# x <- rnorm(n, 50, 25)
# y <- rnorm(n, 50, 25)
# df1 <- mutaframe(X = x, Y = y)
# plot1 <- new_plot(400, 400, xrange = range( df1[,1]), yrange = range( df1[,2]))
# qtpoint(parent = plot1, x = df1[,1], y = df1[,2])
# qtrect(parent = plot1, x = df1[,1], y = df1[,2], color=col2rgb(rgb(1,seq(0,1,length=nrow(df1)),0,0.5),T), width=2,height=2)
# view <- qplotView(plot1$scene)
# print(view)

###End Example###

##############################
###Dependencies###

library(".../Marie/qtpaintANDbase_v425/api_0.1-2.R")

###End Dependencies###

##############################
#draw a point in data coordinates
qtpoint <- function(parent, x, y, color='black', size=5, alpha=1.0){
  layerID <-length(parent$root$childItems()) + 1
  mark <- glyph(left = x - parent$limits$left(), bottom = y - parent$limits$top(), fill = color, stroke = color, size = size, parent = parent)
  add_layer(parent, mark)
  modify_layer(layerID, parent, alpha)
}

#-----------
# draw a rectangle centered at x, y
# uses data coordinates
qtrect <- function(parent, x, y, color="black", width=5, height=5, alpha=1.0){
  layerID <- length(parent$root$childItems()) + 1
  mark <- rect(left = x - ( 0.5 * width ) - parent$limits$left(), bottom= y - ( 0.5 * height ) - parent$limits$top(), width = width, height = height, fill = color, stroke = color, parent = parent)
  add_layer(parent, mark)
  modify_layer(layerID, parent, alpha)
}

