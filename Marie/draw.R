#source("api-sketch.r")
#source("helper.r")

qtscat <- function(x, y, ...,title=NULL, xlab = NULL, ylab = NULL, color="black", fill = NULL, stroke = "black")
{
	ranges <- c(make_data_ranges(x), make_data_ranges(y))
	bprint(ranges)
	
    #create the plot
	#window size 600 x 600; xrange and yrange from above
	windowRanges <- make_window_ranges(ranges, xlab, ylab)
	plot1<-make_new_plot(windowRanges)
	draw_grid(plot1, ranges)         #layer 1 - grid background
	plot1$add_layer(glyph(left = x, bottom = y, fill=color, stroke=stroke))  #layer 2 - data
#	draw_x_axes(plot1, ranges, xlab)
#	draw_y_axes(plot1, ranges, ylab) 
	print(plot1)
	
	
}

#no changes
draw_grid <- function(plotObj, dataRanges)
{
	xGridLines <- make_pretty_axes(dataRanges[1:2], dataRanges[1], dataRanges[2])
	yGridLines <- make_pretty_axes(dataRanges[3:4], dataRanges[3], dataRanges[4])
	draw_grid_with_positions(plotObj, dataRanges, xGridLines, yGridLines)
	
}

#no changes
make_pretty_axes <- function(dataRange, minimum, maximum)
{
	prettyness <- pretty(dataRange)
	prettyness <- prettyness[prettyness >= minimum]
	prettyness <- prettyness[prettyness <= maximum]
	prettyness
}

#draws grid background as a single layer
draw_grid_with_positions <- function(plotObj, dataRange, horiPos=NULL, vertPos=NULL){
	intervalX<-(dataRange[2]-dataRange[1])/(length(horiPos)+1)
	intervalY<-(dataRange[4]-dataRange[3])/(length(vertPos)+1)
	
	left<-round(rep(seq(from=dataRange[1],to=dataRange[2]-intervalX,by=intervalX),length(horiPos)+1),3)
	right<-round(rep(seq(from=dataRange[1]+intervalX-.002,to=dataRange[2],by=intervalX),length(horiPos)+1),3)
	
	bottom_temp<-seq(from=dataRange[3], to=dataRange[4]-intervalY,by=intervalY)
	bottom<-NULL
	for (i in 1:length(bottom_temp)){
		bottom<-c(bottom,rep(bottom_temp[i],(length(vertPos)+1)))
	}
	bottom<-round(bottom,3)
	
	top_temp<-seq(from=dataRange[3]+intervalY-.002,to=dataRange[4],by=intervalY)
	top<-NULL
	for(i in 1:length(top_temp)){
		top<-c(top,rep(top_temp[i],(length(vertPos)+1)))
	}
	top<-round(top,3)
	
	plotObj$add_layer(rect(left=left,right=right,bottom=bottom,top=top,fill="grey80",stroke="grey80"))
	
}

#no changes
draw_x_axes <- function(plotObj, dataRanges, name)
{
	xRangeLabels <- make_pretty_axes(dataRanges[1:2], dataRanges[1], dataRanges[2])
	draw_x_axes_with_labels(plotObj, dataRanges, xRangeLabels, xRangeLabels, name)
	
}

draw_x_axes_with_labels <- function(plotObj, dataRanges, axisLabels, labelHoriPos, name = NULL)
{
#  X label
	x_left <- range(dataRanges[1:2])
	x_bottom <- c(dataRanges[3],dataRanges[3])
	x_bottom <- x_bottom - 0.03 * diff(dataRanges[3:4])
	x_labelpos <- dataRanges[3] - 0.04 * diff(dataRanges[3:4])
	
	plotObj$add_layer(line(left=x_left,bottom=x_bottom,stroke="grey"))
	
	plotObj$add_layer(
					  text(
						   text=axisLabels,
						   left=labelHoriPos,
						   bottom=x_labelpos, 
						   stroke="grey",
						   valign="top"
						   )
					  )
	
	if(!is.null(name))
    plotObj$add_layer(
					  text(
						   text = name,
						   left = x_left[1] + 0.5 * diff(x_left),
						   bottom = dataRanges[3] - 0.13 * diff(dataRanges[3:4]),
						   stroke = "black",
						   valign = "center"
						   
						   )
					  )  
	
	
	
	bprint(x_left)
	bprint(x_bottom)
	bprint(x_labelpos)
	x_axisLabels <- axisLabels
	bprint(x_axisLabels)
	bprint(labelHoriPos)
	
}


#no changes
draw_y_axes <- function(plotObj, dataRanges, name = NULL)
{
	yRangeLabels <- pretty(dataRanges[3:4])
	yRangeLabels <- make_pretty_axes(dataRanges[3:4], dataRanges[3], dataRanges[4])
	draw_y_axes_with_labels(plotObj, dataRanges, as.character(yRangeLabels), yRangeLabels, name)
}

draw_y_axes_with_labels <- function(plotObj, dataRanges, axisLabels, labelVertPos, name = NULL)
{
#  Y label
	y_left <- dataRanges[1] - 0.03 * diff(dataRanges[1:2])
	y_bottom = dataRanges[3:4]
#  print(y_bottom)
#  y_bottom <- range(y_bottom[y_bottom >= 0 && y_bottom < windowRanges[4]])
	y_labelpos = dataRanges[1] - 0.04 * diff(dataRanges[1:2])
	
	
#draw x and y axes!
	plotObj$add_layer(line(left=y_left,bottom=y_bottom,stroke="grey"))
	
	plotObj$add_layer(
					  text(
						   text = axisLabels, 
						   left = y_labelpos, 
						   bottom = labelVertPos, 
						   stroke = "grey",
						   halign = "right"
						   )
					  )
	
	if(!is.null(name))
    plotObj$add_layer(
					  text(
						   text = name,
						   left = dataRanges[1] - 0.13 * diff(dataRanges[1:2]),
						   bottom = y_bottom[1] + 0.5 * diff(y_bottom),
						   stroke = "black",
						   valign = "center"
						   
						   )
					  )  
	
	
	bprint(y_left)
	bprint(y_bottom)
	bprint(y_labelpos)
	y_axisLabels <- axisLabels
	bprint(y_axisLabels)
	bprint(labelVertPos)
	
}

	
		
	
	
	    