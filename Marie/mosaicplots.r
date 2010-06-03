qtmosaic <- function(data, colour="grey30", alpha=1)
{
	top <- data$t
	bottom <- data$b
	left <- data$l
	right <- data$r
	
	ranges <- c(make_data_ranges(c(min(left), max(right))),make_data_ranges(c(min(bottom),max(top))))
#ranges <- ranges + 0.05*c(-1,1,-1,1)
	bprint(ranges)
	
#create the plot
#window size 600 x 600; xrange and yrange from above
	windowRanges <- make_window_ranges(ranges, "x-axis", "y-axis")
#  windowRanges <- ranges + 0.05*c(-1,1,-1,1)
	plot1<-make_new_plot(windowRanges=windowRanges)
	gridRanges<-c(-.1,1.1,-.1,1.1)
	draw_grid(plot1, gridRanges)
#  draw_grid_with_positions(plot1, windowRanges, make_pretty_axes(ranges[1:2], ranges[1], ranges[2]), make_pretty_axes(ranges[3:4], ranges[3], ranges[4]))
	
#    draw_x_axes_with_labels(plot1, ranges, labelNames, labelPos, xlab)
#    draw_y_axes_with_labels(plot1, ranges, labelNames, labelPos, ylab)
	
	plot1$add_layer(rect(top=top,left=left, bottom=bottom, right=right, fill=colour))
	
#browser()
	plot1
}