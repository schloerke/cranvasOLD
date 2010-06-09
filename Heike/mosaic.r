source("Marie/api-sketch.r")
source("Marie/draw.R")
source("Barret/axes.r")
require(stringr)
require(productplots)

find_x_label <- function(df) {
  vars <- setdiff(names(df), c(".wt", "l", "r", "t", "b", "level"))

  axis.set <- subset(df, (b==min(b)) &  (level==max(level)))
  
  paste(vars[sapply(vars, function(x) return(length(unique(axis.set[,x]))>1))],"")
}

find_y_label <- function(df) {
  vars <- setdiff(names(df), c(".wt", "l", "r", "t", "b", "level"))

  axis.set <- subset(df, (l==min(l)) & (level==max(level)))
  
  paste(vars[sapply(vars, function(x) return(length(unique(axis.set[,x]))>1))],"")
}


qtmosaic <- function(data, formula, divider = mosaic(), cascade = 0, scale_max = TRUE, na.rm = FALSE, subset, ...) {
  res <- productplots:::prodcalc(data, formula, divider, cascade, scale_max, na.rm = na.rm)
  
  if (!missing(subset)) {
    sel <- eval(substitute(subset), res, parent.frame())
    res <- res[sel & !is.na(sel), ]
  }
  res <<- res
  qtmosaic.draw(res, ...)
}



qtmosaic.draw <- function(data, colour="grey30", highlight="red", alpha=1) {
  top <- data$t
  bottom <- data$b
  left <- data$l
  right <- data$r

  xlab <- find_x_label(data)
  ylab <- find_y_label(data)

  dataRanges <- c(make_data_ranges(c(min(left), max(right))),make_data_ranges(c(min(bottom),max(top))))
  bprint(dataRanges)

# space in window around plot (margins in base R)  
# this space depends on the labels needed on the left
# find out about these first:

  row <- productplots:::find_row_level(data)
  ylabels <- NULL
  if (!is.na(row))
  	ylabels <- productplots:::row_labels(data[data$level == row, ])

  windowRanges <- make_window_ranges(dataRanges, xlab, ylab, ytickmarks=ylabels)
  # adjust for space needed 
#  windowRanges[1] <- windowRanges[1] - ylabel*diff(windowRanges[1:2])/600

  #create the plot
  #window size 600 x 600
  plot1<-make_new_plot(windowRanges=windowRanges)

 
# draw grid 
  sx <- scale_x_product(data)
  sy <- scale_y_product(data)
  draw_grid_with_positions(plot1, dataRanges, sx$breaks, sy$breaks, sx$minor_breaks, sy$minor_breaks)

# put labels, if appropriate
  col <- productplots:::find_col_level(data)
  if (!is.na(col)) {
  	labels <- productplots:::col_labels(data[data$level == col, ])
	
	draw_x_axes_with_labels(plot1, dataRanges, axisLabel=labels$label, labelHoriPos=labels$pos, name=xlab)
  } else {
  	draw_x_axes_with_labels(plot1, dataRanges, axisLabel=rep("",length(sx$breaks)), labelHoriPos=sx$breaks, name=xlab)
  }

  if (!is.na(row)) {
  	labels <- productplots:::row_labels(data[data$level == row, ])
  	draw_y_axes_with_labels(plot1, dataRanges, axisLabel=labels$label, labelVertPos=labels$pos, name=ylab)
	
  } else {
  	draw_y_axes_with_labels(plot1, dataRanges, axisLabel=rep("",length(sy$breaks)), labelVertPos=sy$breaks, name=ylab)
  }
  

# background layer
  plot1$add_layer(rect(top=top, left=left, bottom=bottom, right=right, fill=colour))

  # for highlighting: draw highlighted boxes
  if (length(grep("highlight",names(res))) > 0) {
	hildata <- subset(data, highlight==T)
	top <- hildata$t
	bottom <- hildata$b
	left <- hildata$l
	right <- hildata$r
#    plot1$add_layer(rect(top=top, left=left, bottom=bottom, right=right, fill=highlight))
  }

#  plot1$add_layer(line( left=rep(sx$breaks,each=3),  bottom=rep(c(dataRanges[3:4],NA),length(sx$breaks)), stroke="yellow"))
  plot1
}

