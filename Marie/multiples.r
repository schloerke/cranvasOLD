##how to draw a graph with multiple element types (glyph,line, text, etc) in one layer

library(qtpaint)

setwd("/Users/marie/Documents/cranvas/Marie")
source("api-sketch.r")
source("draw.r")



#create a data frame obj

df<-data.frame(left=c(0.1,0.2,0.3,0.4),right=c(NA,.8,.7,NA),top=c(NA,.6,.5,NA),bottom=c(.9,.4,.3,.6),
color=c("red","blue","black","green"),shape=c("glyph","rect","line","text"))

#> df
###left right top bottom color shape
#1  0.1    NA  NA    0.9   red glyph
#2  0.2   0.8 0.6    0.4  blue  rect
#3  0.3   0.7 0.5    0.3 black  line
#4  0.4    NA  NA    0.6 green  text


#draw canvas and background grid with padding
gridRanges<-make_window_ranges(c(0,1,0,1))
plot1<-new_plot(width=800,height=600,xrange=gridRanges[1:2],yrange=gridRanges[3:4])
draw_grid(plot1,c(0,1,0,1))

#create mark object
mark<-list()
for (i in 1:(dim(df)[1])){
	if (df[i,6]=="glyph"){
		mark[[i]]<-structure(list(top = NULL, left = df[i,1], right = NULL, bottom = df[i,4],
								fill = df[i,5], stroke = df[i,5]), 
						   class = c("cranvas", "glyph"))
	}
	
	if(df[i,6]=="rect"){
		mark[[i]]<-structure(list(top = df[i,3], left = df[i,1], right = df[i,2], bottom = df[i,4], fill = df[i,5],
								  stroke = df[i,5]), class = c("cranvas", "rect"))
			}
	if(df[i,6]=="line"){
		mark[[i]]<-structure(list(top = NULL, left = df[i,1:2], right = NULL, bottom = c(df[i,4],df[i,3]), fill = df[i,5],
								  stroke = df[i,5]),  class = c("cranvas", "line"))
	}
	if(df[i,6]=="text"){
		mark[[i]]<-structure(list(text = "text", left = df[i,1], bottom = df[i,4], fill = df[i,5], stroke = df[i,5],
								  halign = "center", valign = "center"), class = c("cranvas", "text"))
	}
		
	
}
					
print(mark)

for(i in 1:(dim(df)[1])){
		plot1$add_layer(mark[[i]])
}

print(plot1)