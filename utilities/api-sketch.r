library(qtpaint)
library(plumbr)
#library(plyr)
# Marks: glyph, text, line, rect, poly
# Position: top, right, bottom, left


if (FALSE) {
#source("/home/marie/cranvas/R/api-sketch.r")
	plot1 <- new_plot(500, 500, xrange = c(0, 1), yrange = c(0, 1))
	
	x <- runif(1000)
	y <- runif(1000)
	plot1$add_layer(glyph(bottom = y, left = x))
	for(i in 1:1000) {
		x <- x + runif(1000, min = -0.005, max = 0.005)
		y <- y + runif(1000, min = -0.005, max = 0.005)
		plot1$modify_layer(1, bottom = y, left = x) 
		Sys.sleep(1 / 60)
	}
}

# How should units work?  Definitely need both pixel and data coordinates.
# Some resolution independence with physical units would be desirable 
# long term.

#MV~
# Is there a way to retrieve dpi for the display system? 
# should be able to write a function that translates desired size (eg 4 x 6 in) to pixels and can adjust the plot size accordingly
#~MV



# Mark constructions
# ---------------------
# 
# Each of these functions creates a mark object with just enough information
# to draw it. Eventually these functions should select between (e.g.)
# top/bottom/height and provide more input checking.
#
# In protovis, can specify data, and parameters can be functions of that data.
# This is similar to ggplot2, but in ggplot2 it's easier to specify functions
# than constants.
# 
# glyph(x = 1:10)
# glyph(x = .(mpg), y = .(cyl), data = mtcars)

glyph <- function(top = NULL, left = NULL, bottom = NULL, right = NULL, fill = "black", stroke = NULL, size= 5) {
structure(list(top = top, left = left, right = right, bottom = bottom,
fill = fill, stroke = stroke,size=size), 
class = c("cranvas", "glyph"))
}

rect <- function(top = NULL, left = NULL, bottom = NULL, right = NULL, fill = "black", stroke = NULL) {
structure(list(
top = top, left = left, right = right, bottom = bottom, fill = fill,
stroke = stroke), class = c("cranvas", "rect"))
}

hbar <- function(width = NULL, top = NULL, bottom = NULL, left = 0, fill = "black", stroke = NULL) {
rect(top = top, right = width, bottom = bottom, left = left,
fill = fill, stroke = stroke)
}

vbar <- function(height = NULL, left = NULL, right = NULL, bottom = 0, fill = "black", stroke = NULL) {
rect(top = height, bottom = bottom, left = left, right = right, 
fill = fill, stroke = stroke)
}


line <- function(top = NULL, left = NULL, bottom = NULL, right = NULL, fill = "black", stroke = NULL, width=1) {
structure(list(
top = top, left = left, right = right, bottom = bottom, fill = fill,
stroke = stroke, width=width), class = c("cranvas", "line"))
}

text <- function(top = NULL, left = NULL, bottom = NULL, right = NULL, text = NULL, fill = "black", stroke = NULL ,valign="center",halign="center", rot=0){
structure(list(
text = text, left = left, bottom = bottom, fill = fill, stroke = stroke,
halign = halign, valign = valign, rot = rot), class = c("cranvas", "text"))
}

# Thin wrappers around qtpaint drawing functions that basically translate
# argument names.  (And maybe wrap around any qtpaints that need to be
# temporarily worked around). 
draw <- function(mark, canvas) UseMethod("draw")

draw.glyph <- function(mark, canvas) {
#circle <- qpathCircle(0, 0, mark$size)
circle <- qglyphCircle(r=mark$size)
qdrawGlyph(canvas, circle, x=mark$left, y=mark$bottom,stroke = mark$stroke, fill = mark$fill)
print("size")
print(mark$size)
}

draw.rect <- function(mark, canvas) {
qdrawRect(canvas, mark$left, mark$bottom, mark$right, mark$top,
stroke = mark$stroke, fill = mark$fill)
}

draw.line <- function(mark, canvas) {
qlineWidth(canvas) <- mark$width
qdrawLine(canvas, mark$left, mark$bottom, stroke = mark$stroke)
}

draw.text<-function(mark,canvas){
qstrokeColor(canvas) <- mark$stroke
qdrawText(canvas, text = mark$text, mark$left, mark$bottom,
valign = mark$valign, halign = mark$halign, rot=mark$rot)
}

update.cranvas <- function(object, ...) {
new <- list(...)
structure(defaults(new, object), class = class(object))
}

# Interaction
# 
# Callbacks: keyPress, keyRelease, mouseDoubleClick, mouseMove, mousePress, 
# mouseRelease, wheel
#
# Receive event object.
# Can use qprimitives to get list of things in rectangle
# Do events need to be simplified?

#MV~
# added mousepress place holder

new_plot <- function(width, height, xrange = c(0, 1), yrange = c(0, 1)) {
limits <- qrect(xrange, yrange)
marks <- list()
layers <- mutaframe()
scene <- Qt$QGraphicsScene()
root <- qlayer(scene)
root$geometry<-qrect(0,0,width,height)

add_layer <- function(mark,keyPressFun = NULL, 
keyReleaseFun = NULL, mouseDoubleClickFun = NULL, mouseMoveFun = NULL, 
mousePressFun = NULL, mouseReleaseFun = NULL, wheelFun = NULL, 
hoverMoveEvent = NULL, hoverEnterEvent = NULL, hoverLeaveEvent = NULL, 
contextMenuEvent = NULL, dragEnterEvent = NULL, dragLeaveEvent = NULL, 
dragMoveEvent = NULL, dropEvent = NULL, focusInEvent = NULL, 
focusOutEvent = NULL, sizeHintFun = NULL,row=0L,col=0L, userlimits=NULL,geometry=qrect(0,0,600,400)) {
i <- length(marks) + 1
marks[[i]] <<- mark

if(class(mark)[1]=="function"){
  paintFun<-marks[[1]]
 }else{
  paintFun<-function(item, painter, exposed) { draw(marks[[i]], painter)}
}



layer <- qlayer(parent=root, paintFun=paintFun,keyPressFun=keyPressFun,
  keyReleaseFun=keyReleaseFun,mouseDoubleClickFun=mouseDoubleClickFun,
  mouseMoveFun=mouseMoveFun,mousePressFun=mousePressFun,mouseReleaseFun=mouseReleaseFun,
  wheelFun=wheelFun,hoverMoveEvent=hoverMoveEvent,hoverEnterEvent=hoverEnterEvent,
  hoverLeaveEvent=hoverLeaveEvent,contextMenuEvent=contextMenuEvent,dragEnterEvent=dragEnterEvent,
  dragLeaveEvent=dragLeaveEvent,dragMoveEvent=dragMoveEvent,dropEvent=dropEvent,focusInEvent=focusInEvent,
  focusOutEvent=focusOutEvent,sizeHintFun=sizeHintFun,clip=F, row=row,col=col,geometry=geometry)

#set layer limits by external argument
if(is.null(userlimits)){
	layer$setLimits(limits)
}else {
layer$setLimits(userlimits)
}

layers[[i]] <<- layer   
assign("layers",layers, pos=1) #there has to be a better way for tracking this value, but I don't know what
invisible(self)

}

modify_layer <- function(i,new_mark,new_limit,...) {
old <- marks[[i]]
marks[[i]] <<- new_mark
layers[[i]]$setLimits(new_limit)
qupdate(layers[[i]])
invisible(self)
}

view <- qplotView(scene = scene)


self <- structure(list(
root=root,
layers=layers,
view = view,
add_layer = add_layer,
modify_layer = modify_layer
), class = "cranvas-plot")
self
}

"print.cranvas-plot" <- function(x, ...) print(x$view)

# Higher-level -------------------------------------------------------------
# Something for Marie to work on?

# Should work like R and maintain stack of graphics devices.  Unless you 
# explicitly ask, it should use the most recently opened one

#scatterplot <- function(x, y, ...) {
#  plot1<- new_plot(xrange = range(x), yrange = range(y))
#  plot1$add_layer(glyph(bottom = y, left = x, ...))
#}

# histogram
# add colour, fill, size, shape, etc.
