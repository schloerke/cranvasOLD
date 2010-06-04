source("../Marie/api-sketch.r")
source("../Marie/draw.R")



qtmosaic <- function(data, colour="grey30", highlight="red", alpha=1) {
  top <- data$t
  bottom <- data$b
  left <- data$l
  right <- data$r

  ranges <- c(make_data_ranges(c(min(left), max(right))),make_data_ranges(c(min(bottom),max(top))))
  bprint(ranges)

# adds additional space around the mosaic
  gridRanges <- make_window_ranges(ranges)
# space in window around plot (margins in base R)  
  windowRanges <- make_window_ranges(gridRanges, "x-axis", "y-axis")

  #create the plot
  #window size 600 x 600
  plot1<-make_new_plot(windowRanges=windowRanges)

  draw_grid(plot1, gridRanges)
#  draw_grid_with_positions(plot1, windowRanges, make_pretty_axes(ranges[1:2], ranges[1], ranges[2]), make_pretty_axes(ranges[3:4], ranges[3], ranges[4]))

#    draw_x_axes_with_labels(plot1, ranges, labelNames, labelPos, xlab)
#    draw_y_axes_with_labels(plot1, ranges, labelNames, labelPos, ylab)

# background layer
  plot1$add_layer(rect(top=top, left=left, bottom=bottom, right=right, fill=colour))

  # for highlighting: draw highlighted boxes
  if (length(grep("highlight",names(res))) > 0) {
	hildata <- subset(data, highlight==T)
	top <- hildata$t
	bottom <- hildata$b
	left <- hildata$l
	right <- hildata$r
    plot1$add_layer(rect(top=top, left=left, bottom=bottom, right=right, fill=highlight))
  }

  plot1
}

# Some Examples  
library(productplots)  
prodplot(happy, ~ health+sex+happy, c("vspine","hspine","hspine"), subset=level==3)  
# produces res as a side-effect  

qtmosaic(res)  

res$highlight <- res$sex=="male" 
res$highlight <- is.na(res$happy)


qtmosaic(res, highlight="red")  


# if there is a highlight vector, show it
happy$highlight <-happy$degree=="bachelor"
prodplot(happy, ~ highlight+health+sex+happy, c("vspine","hspine","hspine","hspine"), subset=level==4)  

qtmosaic(res, highlight="red")  
