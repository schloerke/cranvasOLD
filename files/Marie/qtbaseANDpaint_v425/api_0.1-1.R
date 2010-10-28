#qtbase v 0.8-4
#qtpaint v 0.7.9
#svn 425
library(qtpaint)
options(error=recover)

#####################################################################
###In-progress Example###
##may not run

###End in progress###
####################################################################
######################################################################
###Example###

##not run!
#library(plumbr)
#source(".../api_0.1-0.R")

##data
#n<-10000
#x<-rnorm(n,50,25)
#y<-rnorm(n,50,25)
#df1<-mutaframe(X=x,Y=y) 
#width=400
#height=400
#rectW<-2
#rectH<-2

#axes <- function(item, painter) {
#	qfont(painter) <- qfont(pointsize=12)
#	pos <- as.matrix(item$geometry) + 5
#	qdrawText(painter, colnames(df1)[1], pos[2], pos[4], "right", "bottom")
#	qdrawText(painter, colnames(df1)[2], pos[1], pos[3], "left", "top")
#}
#
#mark<- glyph(left=df1[,1],bottom=df1[,2],stroke=NA,fill='black',size=5,xrange=range(df1[,1]),yrange=range(df1[,2]))
#mark2<-rect(left=df1[,1]-0.5*rectW,bottom=df1[,2]-0.5*rectH,height=rectH, width=rectW,stroke=NA,fill=col2rgb(rgb(1,seq(0,1,length=nrow(df1)),0,0.5),T),xrange=range(df1[,1]),yrange=range(df1[,2]))
#mark3<-line(left=50+c(1:90)*sin(6*pi*c(1:90)/100),bottom=50+c(1:90)*cos(6*pi*c(1:90)/100),stroke="red",width=3,xrange=range(df1[,1]),yrange=range(df1[,2]))
#mark4<-hbar(bottom=50,left=c(-9,59),right=c(25,92),xrange=range(df1[,1]),yrange=range(df1[,2]))
#mark5<-vbar(left=50,bottom=c(-33,83),top=c(0,115))
#mark6<-text(top=mean(df1[,1]),left=mean(df1[,2]),text="test",xrange=range(df1[,1]),yrange=range(df1[,2]))

#plot1<-new_plot(width,height,xrange=range(df1[,1]),yrange=range(df1[,2]))

#add_layer(parent=plot1,mark=mark)
#add_layer(parent=plot1,mark=mark2)
#add_layer(parent=plot1,mark=mark3)
#add_layer(parent=plot1,mark=mark4)
#add_layer(parent=plot1,mark=mark5)
#add_layer(parent=plot1,mark=mark6)

#view <- qplotView(scene = plot1$scene)

#overlay<-view$overlay()
#axesOverlay<-qlayer(overlay,axes)
#print(view)

#modify_layer(layerID=1,parent=plot1,show=F)
#modify_layer(layerID=1,parent=plot1,show=T)
#modify_layer(layerID=3,parent=plot1,alpha=0.3)
#modify_layer(layerID=6,parent=plot1,shift_right=200)


###End Example###
######################################################################
######################################################################
###Mark Constructors###
#inspired from protovis 
#TODO: update to pass in xrange and yrange from plot element


#Reference from protovis
#protovis object: dot
#reference: http://vis.stanford.edu/protovis/docs/dot.html
#minimum properties: left & bottom
#top - the distance from the top edge of the parent panel to the dot center.
#left - the distance from the left edge of the parent panel to the dot center.
#bottom - the distance from the bottom edge of the parent panel to the dot center.
#right - the distance from the right edge of the parent panel to the dot center.
#size - the size (proportional to area) of the dot. NOTE: see changes below

#Changes/Additions from protovis
##xrange - (xmin,xmax) of parent panel
##yrange - (ymin,ymax) of parent panel
##size - radius
##stroke - color of glyph outline
##fill - color to fill glyph

glyph <- function(top = NULL, left = NULL, bottom = NULL, right = NULL, fill = "black", stroke = NA, size= 5,xrange,yrange,...) {
#print(left)
#print(right)
#print('&&&')
  if(is.null(left) & !is.null(right)){ ##map value into left position
     left<-xrange[2]-right
  }
  if(!is.null(top) & is.null(bottom)){ ##map value into bottom position
    bottom<-yrange[2]-top
  }
  if(is.null(left) | is.null(bottom)){
    print("need min properties left and bottom")
  }
#print(left)
#print(right)
  #use minimum marks to draw
  structure(list(left = left, bottom = bottom,fill = fill, stroke = stroke,size=size), class = c("cranvas", "glyph"))
}

#Reference from protovis:
#protovis object - bar
#reference - http://vis.stanford.edu/protovis/docs/bar.html
#minimum properties: left,bottom, width, height
#top - the distance from the top edge of the parent panel.
#left - the distance from the left edge of the parent panel.
#bottom - the distance from the bottom edge of the parent panel.
#right - the distance from the right edge of the parent panel.
#width - the width of the bar.
#height - the height of the bar

#Changes/Additions from protovis:
##xrange - (xmin,xmax) of parent panel
##yrange - (ymin,ymax) of parent panel
##fill - color to fill rect
##stroke - color of rect outline

#Comments:
## Do we really need to modify arguments to support protovis naming conventions only to modify them again to support qtpaint property conventions?
##   Wouldn't it be easier to have qtpaint support property conventions? Note this question does not address superficial naming conventions, rather 
##   observing that protovis defines the shape using width and height properties. qdrawRect explicitly uses left right top bottom.

rect <- function(top = NULL, left = NULL, bottom = NULL, right = NULL, width=NULL, height=NULL,fill = "black", stroke = NULL,xrange,yrange) {
  if(!is.null(left) && is.null(width) && !is.null(right)){ ##generate 'width' property from 'left' and 'right' arguments
    width<-right-left
  } else if(is.null(left) && !is.null(width) && !is.null(right)) { ##generate 'left' property from 'right' and 'width' arguments
    left<-right-width
  }

  if(is.null(bottom) && !is.null(height) && !is.null(top)){ ##generate 'bottom' property from 'height' and 'top' arguments
    bottom<-top-height
  }else if(!is.null(bottom) && is.null(height) && !is.null(top)){##generate 'height' property from 'bottom' and 'top' arguments
    height<-top-bottom
  }

  if(is.null(left) | is.null(width) | is.null(bottom) | is.null(height)){
    print("need all attributes left,width,bottom,height")
  }
  ##use minimum marks to draw
  structure(list(left = left, bottom = bottom, right=(left+width),top=(bottom+height),fill = fill,stroke = stroke), class = c("cranvas", "rect"))
}

#Reference from protovis:
#protovis object - line
#reference - http://vis.stanford.edu/protovis/docs/line.html
#minimum properties: left, bottom
#top - the distance from the top edge of the parent panel to the line center.NOTE: see changes below
#left - the distance from the left edge of the parent panel to the line center.NOTE: see changes below
#bottom - the distance from the bottom edge of the parent panel to the line center.NOTE: see changes below
#right - the distance from the right edge of the parent panel to the line center.NOTE: see changes below
#defining the top property instead of bottom, the line is flipped vertically: 
#using right instead of left flips horizontally 

#Changes/Additions from protovis:
## top,left,bottom,right measure distance to segment endpoints #like specs for provis rule (see below)

line <- function(top = NULL, left = NULL, bottom = NULL, right = NULL,stroke = "black", width=1,xrange,yrange) {
  if(is.null(left) && !is.null(right)){ ##flip coordinates left/right depending on which is selected
#    temp<-sum(yrange)
    temp<-max(right, na.rm=TRUE)+min(right,na.rm=TRUE)
    left<-temp-right
  }
 
  if(is.null(bottom) && !is.null(top)){ ##flip coordinates top/bottom depending on which is selected
#    temp<-sum(xrange)
    temp<-max(top, na.rm=TRUE)+min(top,na.rm=TRUE)
    bottom<-temp-top
  }
 
  if(is.null(left) | is.null(bottom)){
    print("need attributes left, bottom")
  }
 structure(list(left = left, bottom = bottom, stroke = stroke, width=width), class = c("cranvas", "line"))
}

#Reference from protovis:
#protovis object - rule
#reference -  http://vis.stanford.edu/protovis/docs/rule.html
#minimum properties:
##horizontal, entire width -bottom
##horizontal, specified width - bottom, left right
##vertical, full height - left
##vertical, specified height - left, top, bottom

hbar <- function(width = 1, top = NULL, bottom = NULL, left = NULL,right=NULL, stroke = 'black',xrange=NULL,yrange=NULL) {
  if(is.null(bottom) && !is.null(top)){
    bottom<-sum(yrange)-top
  }

  if(is.null(left) && is.null(right)){
    left<-xrange
  }else if(!is.null(left) && !is.null(right)) {
    temp<-vector(mode="numeric",length=length(left)*3)
    for (i in 1:length(left)){
      temp[3*i -2]<-left[i]
      temp[3*i-1]<-right[i]
      temp[3*i]<-NA
    }
    left<-temp
  }
 
  if(is.null(bottom) | is.null(left)){
    print("need minimum properties of left and bottom")
  }
  line(bottom=bottom,left=left, width=width,stroke=stroke)
}

vbar <- function(top = NULL, left = NULL, right = NULL, bottom = NULL, stroke = 'black', width=1) {
  if(is.null(left) && !is.null(right)){
    left<-sum(xrange)-right
  }

  if(is.null(top) && is.null(bottom)){
    bottom<-yrange
  }else if(!is.null(top) && !is.null(bottom)){
    temp<-vector(mode="numeric",length=length(bottom)*3)
    for(i in 1:length(bottom)){
      temp[3*i-2]<-bottom[i]
      temp[3*i-1]<-top[i]
      temp[3*i]<-NA
    }
    bottom<-temp
  }
  if(is.null(left) | is.null(bottom)){
    print("need minimum properties of left and bottom")
  }
  line(bottom=bottom,left=left, width=width,stroke=stroke)
}

#Reference from protovis:
#protovis object - labels
#reference - http://vis.stanford.edu/protovis/docs/label.html
#minimum properties: left, bottom, text
#top - the distance from the top edge of the parent panel to the text anchor.
#left - the distance from the left edge of the parent panel to the text anchor.
#bottom - the distance from the bottom edge of the parent panel to the text anchor.
#right - the distance from the right edge of the parent panel to the text anchor.
#textAlign - horizontal alignment.
#textBaseline - vertical alignment.
#textMargin - margin to offset from the text anchor.
#textAngle - rotation angle, in radians.

text <- function(top = NULL, left = NULL, bottom = NULL, right = NULL, text = NULL, stroke = "black" ,valign="center",halign="center", rot=0, margin=NULL, xrange=NULL,yrange=NULL){
  if(is.null(left) && !is.null(right)){
    left<-xrange[2]-right
  }
  if(is.null(bottom) && !is.null(top)){
    bottom<-sum(yrange) - top
  }
  if(is.null(left) | is.null(bottom)){
    print("need min attributes of left and bottom")
  }else{
  #  left<-xrange[2]-left
   # bottom<-bottom-yrange[1]
  }
structure(list(
text = text, left = left, bottom = bottom, stroke = stroke,
halign = halign, valign = valign, rot = rot), class = c("cranvas", "text"))
}

###End Mark Constructors###
######################################################################
#####################################################################
###Draw Wrappers###
# Thin wrappers around qtpaint drawing functions that basically translate
# argument names.  (And maybe wrap around any qtpaints that need to be
# temporarily worked around). 

draw <- function(mark, canvas) UseMethod("draw")

draw.glyph <- function(mark, canvas) {
#circle <- qpathCircle(0, 0, mark$size)
circle <- qglyphCircle(r=mark$size)
qdrawGlyph(canvas, circle, x=mark$left, y=mark$bottom,stroke = mark$stroke, fill = mark$fill)
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

###End Draw Wrappers###
##########################################################
##########################################################
####Canvas & Layers###

#Create a blank canvas
new_plot <- function(width, height, xrange = c(0, 1), yrange = c(0, 1)) {
  limits <- qrect(xrange, yrange)
  #marks <- list()
  #layers <- mutaframe()
  scene <- Qt$QGraphicsScene()
  root <- qlayer(scene)
  root$geometry<-qrect(0,0,width,height)



  self <- structure(list(scene=scene,
                        root=root,
			limits=limits
			#view = view
			#add_layer = add_layer,
#			modify_layer = modify_layer
), class = "cranvas-plot")
  self
}
#End blank canvas

#add layers to the canvas
add_layer<-function(	parent,
			mark,
			keyPressFun = NULL, 
			keyReleaseFun = NULL, 
			mouseDoubleClickFun = NULL, 
			mouseMoveFun = NULL, 
			mousePressFun = NULL, 
			mouseReleaseFun = NULL, 
			wheelFun = NULL, 
			hoverMoveFun = NULL, 
			hoverEnterFun = NULL, 
			hoverLeaveFun = NULL, 
			contextMenuFun = NULL, 
			dragEnterFun = NULL, 
			dragLeaveFun = NULL, 
			dragMoveFun = NULL, 
			dropFun = NULL, 
			focusInFun = NULL, 
			focusOutFun = NULL, 
			sizeHintFun = NULL,
			row=0L,col=0L, 
			userlimits=NULL,
			geometry=qrect(0,0,width,height)){

 if(class(mark)[1]=="function"){
   paintFun<-mark
 }else{
   paintFun<-function(item, painter, exposed) { draw(mark, painter)}
 }


if(is.null(userlimits)){
  limits<-parent$limits
}else {
  limits<-userlimits
}

layer<- qlayer(	parent=parent$root, 
			paintFun=paintFun,
			keyPressFun=keyPressFun,
  			keyReleaseFun=keyReleaseFun,
			mouseDoubleClickFun=mouseDoubleClickFun,
  			mouseMoveFun=mouseMoveFun,
			mousePressFun=mousePressFun,
			mouseReleaseFun=mouseReleaseFun,
  			wheelFun=wheelFun,
			hoverMoveFun = hoverMoveFun, 
			hoverEnterFun = hoverEnterFun, 
			hoverLeaveFun = hoverLeaveFun, 
			contextMenuFun = contextMenuFun, 
			dragEnterFun = dragEnterFun, 
			dragLeaveFun = dragLeaveFun, 
			dragMoveFun = dragMoveFun, 
			dropFun = dropFun, 
			focusInFun = focusInFun, 
			focusOutFun = focusOutFun, 
			sizeHintFun = sizeHintFun,
			clip=F,
			limits=limits,
			row=row,col=col,geometry=geometry)

  
}
#End add_layer

#modify a layer
modify_layer<-function(		layerID,
							parent,
							show=NULL, #true or false
							enabled=NULL, #true (allows interaction) or false (no interaction, faded appearance)
							alpha=NULL, #ranges from 0.0(transparent) to 1.0 (opaque)
                            shift_right=NULL, #in canvas coordinates, see width above
							shift_down=NULL #in canvas coordinates, see height above
){

  if(!is.null(show) && show==TRUE){
    parent$root$childItems()[[layerID]]$show()
  }else if (!is.null(show) && show==F){
    parent$root$childItems()[[layerID]]$hide()
  }

  if(!is.null(enabled)){
    parent$root$childItems()[[layerID]]$setEnabled(enabled)
  }

  if(!is.null(alpha)){
    parent$root$childItems()[[layerID]]$setOpacity(alpha)
  }
  
  if(!is.null(shift_right)){
    parent$root$childItems()[[layerID]]$setX(shift_right)
  }

  if(!is.null(shift_down)){
    parent$root$childItems()[[layerID]]$setY(shift_down)
  }
}

    

#"print.cranvas-plot" <- function(x, ...) print(x$view)

