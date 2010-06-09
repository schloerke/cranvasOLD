#source("api-sketch.r")


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
	return(plot1)
	
	
}

#no changes
draw_grid <- function(plotObj, dataRanges,row=0L,col=0L)
{
	xGridLines <- make_pretty_axes(dataRanges[1:2], dataRanges[1], dataRanges[2])
	yGridLines <- make_pretty_axes(dataRanges[3:4], dataRanges[3], dataRanges[4])
	draw_grid_with_positions(plotObj, dataRanges, xGridLines, yGridLines,row,col)
	
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
draw_grid_with_positions <- function(plotObj, dataRange, horiPos=NULL, vertPos=NULL,row=0L,col=0L){
	dims<-get_dims(dataRange,size=0.002,horiPos,vertPos)
    left<-get_left(dataRange,dims[3],vertPos)
	right<-get_right(dataRange,dims[3],dims[1],vertPos)
	bottom<-get_bottom(dataRange,dims[4],horiPos)
	top<-get_top(dataRange,dims[4],dims[2],horiPos)
	plotObj$add_layer(rect(left=left,right=right,bottom=bottom,top=top,fill="grey90",stroke="grey90"),row=row,col=col)

}

get_dims<-function(dataRange,size=0.002,horiPos=NULL,vertPos=NULL){
	return(c(get_linewidth(data=dataRange[1:2],size=0.002),
			 get_linewidth(data=dataRange[3:4],size=0.002),
			 get_interval(data=dataRange[1:2],number=horiPos),
			 get_interval(data=dataRange[3:4],number=vertPos)))
}
	
	
	
	
get_interval<-function(data,number=NULL){
	return((data[2]-data[1])/(length(number)+1))
}

get_linewidth<-function(data,size=0.002){
	return(round(size*(data[2]-data[1]),3))
}

get_left<-function(dataRange,intervalX,vertPos){
	return(round(rep(seq(from=dataRange[1],to=dataRange[2]-intervalX,by=intervalX),length(vertPos)+1),3))
}

get_right<-function(dataRange,intervalX,linewidthX,vertPos){
	return(round(rep(seq(from=dataRange[1]+intervalX-linewidthX,to=dataRange[2],by=intervalX),length(vertPos)+1),3))
}

get_bottom<-function(dataRange,intervalY,horiPos){
	temp<-seq(from=dataRange[3], to=dataRange[4]-intervalY,by=intervalY)
	bottom<-NULL
	for (i in 1:length(temp)){
		bottom<-c(bottom,rep(temp[i],(length(horiPos)+1)))
	}
	return(round(bottom,3))
}

get_top<-function(dataRange, intervalY,linewidthY,horiPos){
	temp<-seq(from=dataRange[3]+intervalY-linewidthY,to=dataRange[4],by=intervalY)
	top<-NULL
	for(i in 1:length(temp)){
		top<-c(top,rep(temp[i],(length(horiPos)+1)))
	}
	return(round(top,3))
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

#allows plotsize to be designated
make_new_plot <- function(x=600,y=600,windowRanges){
	new_plot(x,y, xrange=windowRanges[1:2], yrange=windowRanges[3:4])
}	

#no changes
make_data_ranges<-function(dataColumn){
#  return(c(min(dataColumn)-0.25, max(dataColumn)+0.25))
	return(c(min(dataColumn)-0.01, max(dataColumn)+0.01))
#  return(range(dataColumn))
}

#no changes
bprint <- function(...) {
	if(if_bprint()) {
		cat(substitute(...),":\n")
		str(...)
	}
}
if_bprint <- function()
{
	return(TRUE)
}
make_window_ranges <- function(dataRanges, xlab=NULL, ylab=NULL)
{
	
	if(!is.null(ylab))
    xmin = dataRanges[1] - 0.2*diff(dataRanges[1:2])
	else
    xmin = dataRanges[1] - 0.13*diff(dataRanges[1:2])
	
	xmax = dataRanges[2]+0.1*diff(dataRanges[1:2])
	
	
	if(!is.null(xlab))
    ymin = dataRanges[3]-0.2*diff(dataRanges[3:4])
	else
    ymin = dataRanges[3]-0.13*diff(dataRanges[3:4])
    
	ymax = dataRanges[4]+0.1*diff(dataRanges[3:4])
	
	
	windowRanges <- c(
					  xmin,
					  xmax,
					  ymin,
					  ymax
					  )
	
	
	if(if_bprint())
    cat("Window Range: x=c(", windowRanges[1],", ",windowRanges[2],")  y=(",windowRanges[3],", ",windowRanges[4],")\n")
	
	windowRanges  
}
	
	    