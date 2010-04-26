library(qtpaint)
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

glyph <- function(top = NULL, left = NULL, bottom = NULL, right = NULL, fill = "black", stroke = NULL) {
  structure(list(top = top, left = left, right = right, bottom = bottom,
    fill = fill, stroke = stroke), 
    class = c("cranvas", "glyph"))
}

rect <- function(top = NULL, left = NULL, bottom = NULL, right = NULL, fill = "black", stroke = NULL) {
  structure(list(
    top = top, left = left, right = right, bottom = bottom, fill = fill,
    stroke = stroke), class = c("cranvas", "rect"))
}

hbar <- function(width = NULL, top = NULL, bottom = NULL, fill = "black", stroke = NULL) {
  rect(top = top, right = width, bottom = bottom, left = 0,
    fill = fill, stroke = stroke)
}

vbar <- function(height = NULL, left = NULL, right = NULL, fill = "black", stroke = NULL) {
  rect(top = height, bottom = 0, left = left, right = right, 
    fill = fill, stroke = stroke)
}


line <- function(top = NULL, left = NULL, bottom = NULL, right = NULL, fill = "black", stroke = NULL) {
  structure(list(
    top = top, left = left, right = right, bottom = bottom, fill = fill,
    stroke = stroke), class = c("cranvas", "line"))
}

text <- function(top = NULL, left = NULL, bottom = NULL, right = NULL, text = NULL, fill = "black", stroke = NULL ,valign="center",halign="center"){
  structure(list(
    text = text, left = left, bottom = bottom, fill = fill, stroke = stroke,
    halign = halign, valign = valign), class = c("cranvas", "text"))
}

# Thin wrappers around qtpaint drawing functions that basically translate
# argument names.  (And maybe wrap around any qtpaints that need to be
# temporarily worked around). 
draw <- function(mark, canvas) UseMethod("draw")

draw.glyph <- function(mark, canvas,size=2) {
  #circle <- qpathCircle(0, 0, size)
  circle <- qglyphCircle()
  qdrawGlyph(canvas, circle, x=mark$left, y=mark$bottom, cex = mark$size, 
    stroke = mark$stroke, fill = mark$fill)
}

draw.rect <- function(mark, canvas) {
  qdrawRect(canvas, mark$left, mark$bottom, mark$right, mark$top,
    stroke = mark$stroke, fill = mark$fill)
}

draw.line <- function(mark, canvas) {
  qdrawLine(canvas, mark$left, mark$bottom, stroke = mark$stroke)
}

draw.text<-function(mark,canvas){
  qstrokeColor(canvas) <- mark$stroke
  qdrawText(canvas, text = mark$text, mark$left, mark$bottom,
    valign = mark$valign, halign = mark$halign)
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
# Recieve event object.
# Can use qprimitives to get list of things in rectangle
# Do events need to be simplified?

#MV~
# added mousepress place holder

new_plot <- function(width, height, xrange = c(0, 1), yrange = c(0, 1)) {
  limits <- qrect(xrange, yrange)
  marks <- list()
  layers <- list()
  scene <- Qt$QGraphicsScene()
  root <- qlayer(scene)
  

  add_layer <- function(mark) {
    i <- length(marks) + 1
    marks[[i]] <<- mark

    layer <- qlayer(root, function(item, painter, exposed) {
      draw(marks[[i]], painter)
    })
    layer$setLimits(limits)
    layers[[i]] <<- layer    
    invisible(self)
  }

  modify_layer <- function(i, ...) {
    old <- marks[[i]]
    new <- update(old, ...)
    marks[[i]] <<- new
    
    qupdate(layers[[i]])
    invisible(self)
  }

  view <- qplotView(scene = scene)

  self <- structure(list(
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
