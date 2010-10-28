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

#------------
# draw a line connecting points (x1,y1), (x2,y2),...
# uses data coordinates
qtline <- function( parent, x, y, color = "black", width = 1, alpha = 1.0) {
  mark <- line(left = x - parent$limits$left(), bottom = y - parent$limits$top(), stroke = color, width = width, parent = parent)
  add_layer(parent, mark)
  modify_layer(length(parent$root$childItems()), parent, alpha)
}

#----------
# draw a horizontal line across entire canvas 
qthline <- function( parent, position, color = "black", width = 1, alpha = 1.0) {
  mark <- hbar(bottom = position - parent$limits$top(), left = parent$limits$left() - parent$limits$left(), right = parent$limits$right() - parent$limits$left(), stroke = color, width = width, parent = parent)
  add_layer(parent,mark)
  modify_layer(length(parent$root$childItems()), parent, alpha)
}

#---------
# draw a vertical line across entire canvas
qtvline <- function (parent, position, color = "black", width = 1, alpha = 1.0) {
  mark <- vbar(left = position - parent$limits$left(), bottom = parent$limits$top() - parent$limits$top(), top = parent$limits$bottom() - parent$limits$top(), stroke = color, width = width, parent = parent)
  add_layer(parent, mark)
  modify_layer(length(parent$root$childItems()), parent, alpha)
}

