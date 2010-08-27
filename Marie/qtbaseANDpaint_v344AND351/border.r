#source("api-sketch.r")

#possible bug that when data range is ~c(0,1) a non-drawable border region is added
#this prevents addition of axes and axes labels
#examples follow

###EXAMPLE 1
#data is outside of c(0,1) range

x<-runif(1000,min=-50,max=50)
y<-runif(1000,min=-50,max=50)

#create canvas area larger than data range
#padding of 5
plot1<-new_plot(500,500,xrange=c(-55,55),yrange=c(-55,55))
print(plot1)

#add layers
plot1$add_layer(glyph(bottom=x,left=y,fill="red")) #data
plot1$add_layer(line(left=c(-51,-51),bottom=c(-55,55),stroke="black"))#yaxis
plot1$add_layer(line(left=c(-55,55),bottom=c(-51,-51),stroke="black"))#xaxis

labels<-seq(from=-55,to=55,by=5)
plot1$add_layer(text(text=labels,left=labels,bottom=-52,stroke="black",valign="top"))
plot1$add_layer(text(text=labels,left=-51,bottom=labels,stroke="black",halign="right"))


##EXAMPLE 2
#data is within range of c(0,1)

x2<-runif(1000)
y2<-runif(1000)

#create canvas area larger than data range
#padding of 0.1
plot2<-new_plot(500,500,xrange=c(-0.1,1.1),yrange=c(-.1,1.1))
print(plot2)

#add layers
##NOTE: border is not proportional to padding. Why?
plot2$add_layer(glyph(bottom=x2,left=y2,fill="red")) #data is drawn with a border
plot2$add_layer(line(left=c(-.05,-.05),bottom=c(-.1,1),stroke="black"))# doesn't display
plot2$add_layer(line(left=c(-.1,1),bottom=c(-.05,-.05),stroke="black"))# doesn't display


##EXAMPLE 3
#data is within range c(0,1) AND we don't try to draw an oversized canvas
x3<-runif(1000)
y3<-runif(1000)

#create canvas
plot3<-new_plot(500,500,xrange=c(0,1),yrange=c(0,1))
print(plot3)

#add layers
plot3$add_layer(glyph(bottom=x3,left=y3,fill="red")) #no border


