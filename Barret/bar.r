source("../utilities/api-sketch.r")
source("../utilities/axes.r")
source("../utilities/helper.r")
source("bprint.r")


#' Create a histogram
#' Create a histogram from numeric data
#'
#' @param data vector of numeric data to be made into a histogram
#' @param horizontal boolean to decide if the bars are horizontal or vertical
#' @param ... arguments supplied to hist() or the hist layer
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  # toture
#'    qthist(rnorm(1000000), floor(rnorm(1000000)*3))
#'    qthist(rnorm(1000000), floor(runif(1000000)*15), title = "Toture - stack") # each column is split evenly
#'    qthist(rnorm(1000000), floor(runif(1000000)*15), title = "Toture - dodge", position = "dodge") # each column has similar height colors
#'    qthist(rnorm(1000000), floor(runif(1000000)*15), title = "Toture - relative", position = "relative") # range from 0 to 1
#'  # color tests
#'    qthist(mtcars$disp, horizontal = TRUE, fill = "gold", stroke = "red4")
#'    qthist(mtcars$disp, mtcars$cyl, stroke = "black")
#'    qthist(mtcars$disp, mtcars$cyl, position = "dodge", stroke = "black")
qthist <- function(
  data, 
  splitBy = rep(1, length(data)), 
  horizontal = FALSE, 
  position = "none", 
  color = NULL, 
  fill = NULL,
  stroke = NULL,
  title = NULL, 
  name = names(data),
  ...
) {
  
  d <- suppressWarnings(hist(data,plot=FALSE,...))
  breaks <- d$breaks
#  bprint(breaks)
  break_len <- length(breaks)
  bar_top <- table(cut(data, breaks = breaks), splitBy)  
#  bprint(bar_top)
  
  bar_bottom <- array(0, dim(bar_top))
  label_names <- dimnames(bar_top)[[1]]
  split_names <- dimnames(bar_top)[[2]]
#  bprint(label_names)
#  bprint(split_names)
    
  if (position == "dodge") {
    pos <- make_dodge_pos( breaks, length(split_names))
    bar_left  <- pos$start
    bar_right <- pos$end
    bar_top <- apply(bar_top, 1, rbind)
        
    color <- rep(color, length(label_names))
  }
  else  {
    # (position == "stack" || position == "relative")
    
    color <- rep(color, each = length(split_names))

    #(position = "stack")
    bar_left <- rep(breaks[1:(break_len-1)], length(split_names))
    bar_right <- rep(breaks[2:break_len] , length(split_names))
    
    # make the bar_top be stacked (cumulative)
    for (i in 1:nrow(bar_top)) {
      bar_top[i,] <- cumsum(bar_top[i,])
    }
    
    
    #make the bar_bottom "stack"
    if (ncol(bar_bottom) > 1) {
      bar_bottom[,2:ncol(bar_bottom)] <- bar_top[,1:(ncol(bar_top) - 1)]
    }
      
    bar_bottom[,1] <- 0
    
    # spine-o-gram      
    if (position == "relative") {
      for (i in 1:nrow(bar_bottom)) {
        bar_bottom[i,] <- bar_bottom[i,] / max(bar_top[i,])
      }

      for (i in 1:nrow(bar_top)) {
        bar_top[i,] <- bar_top[i,] / max(bar_top[i,])
      }
    }
  }
  
    
#  bprint(bar_left)
#  bprint(bar_right)
#  bprint(bar_top)
#  bprint(bar_bottom)

  if (is.null(color)) {
    if (length(split_names) == 1) {
      color <- "grey20"
    } else {
      if(position == "dodge") {
        color <- rep(rainbow(length(unique(splitBy))), length(label_names))        
      } else {
        color <- rep(rainbow(length(unique(splitBy))), each = length(label_names))
      }
    }
  }
  
  if (is.null(stroke)) {
    stroke = color
  }
  if (is.null(fill)) {
    fill = color
  }
    
  # contains c(x_min, x_max, y_min, y_max)
  if (horizontal) {
    ranges <- c(make_data_ranges(c(0, bar_top)), make_data_ranges(breaks))
  } else {
    ranges <- c(make_data_ranges(breaks), make_data_ranges( c(0, bar_top)))
  }
#  bprint(ranges)

  if (horizontal) {
    ylab = name
    xlab = "count"
  } else {
    ylab = "count"
    xlab = name
  }
#  bprint(xlab)
#  bprint(ylab)

  #create the plot
  #window size 600 x 600; xrange and yrange from above
  windowRanges <- make_window_ranges(ranges, xlab, ylab)
  plot1<-make_new_plot(windowRanges)

  #draw grid
  if(horizontal)
    draw_grid_with_positions(plot1, ranges, make_pretty_axes(ranges[1:2], ranges[1], ranges[2]), NULL)
  else
    draw_grid_with_positions(plot1, ranges, NULL, make_pretty_axes(ranges[3:4], ranges[3], ranges[4]))
    
  
  #for different representations of the data (shape, color, etc) pass vecor arguments for shape, color, x, y
#  if(horizontal)
#    plot1$add_layer(hbar(bottom = bar_left, top = bar_right, width = bar_top, ...))
#  else
#    plot1$add_layer(vbar(left = bar_left, right = bar_right, height = bar_top, ...))

  # c(obj) makes a matrix into a vector
  if(horizontal)
    plot1$add_layer(hbar(bottom = c(bar_left), top = c(bar_right), width = c(bar_top), left = c(bar_bottom), fill=fill, stroke = stroke))
  else
    plot1$add_layer(vbar(left = c(bar_left), right = c(bar_right), height = c(bar_top), bottom = c(bar_bottom), fill=fill, stroke = stroke))

  draw_x_axes(plot1, ranges, xlab)
  draw_y_axes(plot1, ranges, ylab) 

  if(!is.null(title))
    add_title(plot1, ranges, title)

  plot1
}


#' Make dodge positions
#'
#' @param breaks break positions
#' @param n number of items per break
#' @keywords internal
#' @author Barret Schloerke
#' @examples
#'  make_dodge_pos(c(1:5), 3)
make_dodge_pos <- function(breaks, n) {
  gap <- diff(breaks[1:2])
  breaks <- breaks[-length(breaks)]
  relPos <- seq(from = gap*.1, to = gap * .9, length.out = n+1)
  startRel <- relPos[-(n+1)]
  endRel <- relPos[-1]

  starts <- c(sapply(breaks, function(x) { 
    x + startRel
  }))
  ends <- c(sapply(breaks, function(x) { 
    x + endRel
  }))

  data.frame(start = starts, end = ends)  
}


#' Create a histogram
#' Create a histogram from numeric data
#'
#' @param data vector of numeric data to be made into a histogram
#' @param horizontal boolean to decide if the bars are horizontal or vertical
#' @param ... arguments supplied to hist() or the hist layer
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  m <- mtcars$cyl 
#'  levels(m) <- c("four", "six", "eight")
#'  str(m)
#'  qtbar(m)
#'  qtbar(m, TRUE, fill = "gold", stroke = "red4")
#'  qtbar(diamonds$color, fill="red4", stroke="gold",horizontal=TRUE,name="Color", title = "diamonds$color")
#'  qtbar(diamonds$color, title="Diamonds",splitBy=diamonds$cut)
#'  qtbar(diamonds$color, title="Diamonds",name = "color", splitBy=diamonds$cut,position="dodge")
#
#qtbar <- function(data, splitBy = rep(1,length(data)), horizontal = FALSE, position = "none", color = rainbow(length(unique(splitBy))), title=NULL, name = names(data))
#{
#  
#  counts <- table(data,splitBy)
#  bprint(counts)
#  bottoms <- array(0, dim(counts))
#  labelNames <- dimnames(counts)[[1]]
#  colorNames <- dimnames(counts)[[2]]
# 
#  bLength <- nrow(counts)
#  labelPos <- 1:bLength - 0.5
# 
# 
#  if (position == "dodge")
#  {
#    start <- make_dodge_start( 0:(bLength-1), length(colorNames) )
#    end <-   make_dodge_end( 0:(bLength-1), length(colorNames) )
#    counts <- apply(counts, 1, rbind)
#    labelNames <- rep(labelNames, length(colorNames))
#   counts <- counts
#    color <- color
#  }
#  else if(position == "stack" || position == "relative")
#  else
#  {
#    (position = "stack")
#    
#    start <- 0:(bLength-1)+.1
#    end <- 1:bLength-.1
#    
#     make the counts be stacked (cumulative)
#    for(i in 1:nrow(counts))
#      counts[i,] <- cumsum(counts[i,])
#    
#    make the bottoms "stack"
#    if(ncol(bottoms) > 1)
#      bottoms[,2:ncol(bottoms)] <- counts[,1:(ncol(counts) - 1)]
#      
#    bottoms[,1] <- 0
#    color <- rep(color, each = length(labelNames))
#    
#    if(position == "relative")
#    {
#      spine-o-gram
#      
#      for(i in 1:nrow(bottoms))
#        bottoms[i,] <- bottoms[i,] / max(counts[i,])
#
#      for(i in 1:nrow(counts))
#        counts[i,] <- counts[i,] / max(counts[i,])
#
#        
#      print(counts)
#      print(bottoms)
#    }
#    
#
#  }
#  
#  make counts a vector
#  counts <- c(counts, NULL)
#  
#  bprint(start)
#  bprint(end)
#  bprint(counts)
#
#   contains c(x_min, x_max, y_min, y_max)
#  if(horizontal)
#    ranges <- c( make_data_ranges(c(0,counts * 1.1)), make_data_ranges(0:bLength))
#  else
#    ranges <- c( make_data_ranges(0:bLength), make_data_ranges(c(0,counts * 1.1)))
#
#  bprint(ranges)
#  
#  if(horizontal)
#  {
#    ylab = name
#    xlab = "count"
#  }
#  else
#  {
#    ylab = "count"
#    xlab = name
#  }
#
#  create the plot
#  window size 600 x 600; xrange and yrange from above
#  windowRanges <- make_window_ranges(ranges, xlab, ylab)
#  plot1<-make_new_plot(windowRanges)
#  
#  draw grid
#  if(horizontal)
#    draw_grid_with_positions(plot1, ranges, make_pretty_axes(ranges[1:2], ranges[1], ranges[2]), NULL)
#  else
#    draw_grid_with_positions(plot1, ranges, NULL, make_pretty_axes(ranges[3:4], ranges[3], ranges[4]))
#
#  for different representations of the data (shape, color, etc) pass vecor arguments for shape, color, x, y
#  if(length(colorNames) == 1)
#    color <- "grey20"
#    
#  if(horizontal)
#    plot1$add_layer(hbar(bottom = start, top = end, width = counts,left = bottoms, fill=color))
#  else
#    plot1$add_layer(vbar(left = start, right = end, height = counts, bottom = bottoms, fill=color))
#
#  # add axes
#  if(horizontal)
#  {
#    draw_x_axes(plot1, ranges, xlab)
#    draw_y_axes_with_labels(plot1, ranges, labelNames, labelPos, ylab)
#  }
#  else
#  {
#    draw_x_axes_with_labels(plot1, ranges, labelNames, labelPos, xlab)
#    draw_y_axes(plot1, ranges, ylab)    
#  }
#  
#  if(!is.null(title))
#    add_title(plot1, ranges, title)
#
#  plot1
#}


#qt_barhist <- function(
#
#  d,
#  splitBy=NULL,
#  horizontal = FALSE,
#
#  xRange,
#  yRange,
#  
#  rect_top,
#  rect_bottom,
#  rect_left,
#  rect_right,
#  rect_fill,
#  rect_stroke,
#  
#  
#  xName,
#  xLabelPos,
#  xLabelNames,
#
#  yName,
#  yLabelPos,
#  yLabelNames,
#  
#  
#  
#  
#  title)
#  
#{
#
##  aesStuff <- aes(...)
##  bprint(aesStuff)
#  
#
#  # contains c(x_min, x_max, y_min, y_max)
#  if(horizontal)
#    ranges <- c(yRange, xRange)
#  else
#    ranges <- c(xRange, yRange)
#  bprint(ranges)
#
#
#  if(horizontal)
#  {
#    ylab = xName
#    xlab = yName
#  }
#  else
#  {
#    ylab = yName
#    xlab = xName
#  }
#
#  #create the plot
#  #window size 600 x 600; xrange and yrange from above
#  windowRanges <- make_window_ranges(ranges, xlab, ylab)
#  plot1<-make_new_plot(windowRanges)
#
#
#  #draw grid
#  if(horizontal)
#    draw_grid_with_positions(plot1, ranges, make_pretty_axes(ranges[1:2], ranges[1], ranges[2]), NULL)
#  else
#    draw_grid_with_positions(plot1, ranges, NULL, make_pretty_axes(ranges[3:4], ranges[3], ranges[4]))
#    
#  
#  #for different representations of the data (shape, color, etc) pass vecor arguments for shape, color, x, y  
##  for(i in unique(rect_stroke))
##  {
##    for(j in unique(rect_fill))
##    {
##      goodRows <- rect_stroke == i & rect_fill == j
#
#      plot1$add_layer(
#        rect(
#          top = rect_top,
#          left = rect_left,
#          bottom = rect_bottom,
#          right = rect_right,
#          fill = rect_fill,
#          stroke = rect_stroke
#        )
#      )
##    }
##  }
#
#  draw_x_axes(plot1, ranges, xlab)
#  draw_y_axes(plot1, ranges, ylab) 
#
#  if(!is.null(title))
#    add_title(plot1, ranges, title)
#
#  plot1
#}