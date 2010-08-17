library(qtpaint)
library(plumbr)

#mac
#setwd("/Users/marie/Documents/cranvas/Marie/")
#linux
setwd("/home/marie/Documents/cranvas/Marie/")
#source("faceting.R")
source("api-sketch.r")
source("draw.R")

#from cranvas/Hadley/linked.r
#mark element for below
draw_points <- function(data, x, y, selected = FALSE, colour = "black") {
	circle <- qglyphCircle(3)
	print("start")
	print(selected)
	function(item, painter, exposed) {
		if (selected) {
			data <- data[data$.selected, ]
			print(data)
		}
		
		
		qfillColor(painter) <- colour
		qstrokeColor(painter) <- NA
		
		qdrawGlyph(painter, circle, data[[x]], data[[y]])  
	}
}




#create the mutable data frame
mtcarsm<-as.mutaframe(mtcars)
mtcarsm$.selected<-FALSE


###
#example 1 simple linking between multiple screens
#looks at mpg as a function of weight or horsepower

#some variables
margin<-5
adjust<-c(margin,-margin)
lims1<-qrect(range(mtcars$wt),range(mtcars$mpg))


#canvas #1
axes1<-function(item,painter){
	qfont(painter)<-qfont(pointsize=12)
	pos<-as.matrix(item$geometry)+adjust
	qdrawText(painter,"wt", pos[2],pos[4],"right","bottom")
	qdrawText(painter,"mpg",pos[1],pos[3],"left","top")
}



scene1 <- qscene()
root1 <- qlayer(scene1)
root1$setLimits(qrect(range(mtcars$wt), range(mtcars$mpg)))
view1 <- qplotView(scene = scene1)
overlay1<-view1$overlay()

points1 <- qlayer(root1, draw_points(mtcarsm, "wt", "mpg"))
selected1 <- qlayer(root1, draw_points(mtcarsm, "wt", "mpg", selected = T, 
colour = "red"))
axesOverlay<-qlayer(overlay1,axes1)

view1$geometry<-qrect(c(0,400),c(0,600))

print(view1)

#canvas #2
axes2<-function(item,painter){
	qfont(painter)<-qfont(pointsize=12)
	pos<-as.matrix(item$geometry)+adjust
	qdrawText(painter,"hp", pos[2],pos[4],"right","bottom")
	qdrawText(painter,"mpg",pos[1],pos[3],"left","top")
}

scene2 <- qscene()
root2 <- qlayer(scene2)
root2$setLimits(qrect(range(mtcars$hp), range(mtcars$mpg)))
view2 <- qplotView(scene = scene2)
overlay2<-view2$overlay()


points2 <- qlayer(root2, draw_points(mtcarsm, "hp", "mpg"))
selected2 <- qlayer(root2, draw_points(mtcarsm, "hp", "mpg", selected = T, 
colour = "blue"))
axesOverlay<-qlayer(overlay2,axes2)

view2$geometry<-qrect(c(0,400),c(0,600))

print(view2)


add_listener(mtcarsm, function(i, j) qupdate(c(selected1,selected2)))


###
#example 2 - in api format

plot1<-new_plot(400,600,xrange=range(mtcars$wt),yrange=range(mtcars$mpg))
plot1$add_layer(mark=glyph(bottom=mtcars$mpg,left=mtcars$wt))

plot1$add_layer(mark=draw_points(mtcarsm,"wt","mpg",selected=T,colour="red"))

print(plot1)
