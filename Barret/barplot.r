source("api-sketch_021910.r")
source("Barret/axes.r")
source("Barret/helper.r")

qtscat <- function(x, y, ...,title=NULL, xlab = NULL, ylab = NULL, color=NULL, fill = NULL, stroke = NULL)
{
  
  
  
  ranges <- c(make_data_ranges(x), make_data_ranges(y))
  bprint(ranges)


  #create the plot
  #window size 600 x 600; xrange and yrange from above
  windowRanges <- make_window_ranges(ranges, xlab, ylab)
  plot1<-make_new_plot(windowRanges)


  #draw grid
  draw_grid(plot1, ranges)
  
  #for different representations of the data (shape, color, etc) pass vecor arguments for shape, color, x, y
  if(is.null(color))
    color = "black"
  if(is.null(stroke))
    stroke = color
  if(is.null(fill))
    fill = color
    
  plot1$add_layer(glyph(left = x, bottom = y, fill=fill, stroke=stroke))

  draw_x_axes(plot1, ranges, xlab)
  draw_y_axes(plot1, ranges, ylab) 
  
  if(!is.null(title))
    add_title(plot1, ranges, title)

  plot1  
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
#'  qthist(rnorm(100000))
#'  qthist(mtcars$disp, TRUE, fill = "gold", stroke = "red4")
qthist <- function(data, horizontal = FALSE, ..., title=NULL, name = names(data))
{

#  aesStuff <- aes(...)
#  bprint(aesStuff)
  
  d <- suppressWarnings(hist(data,plot=FALSE,...))
#  d <- hist(data,plot=FALSE,...)
  bLength <- length(d$breaks)
  
  
  counts <- d$counts  
  start <- d$breaks[1:(bLength-1)]
  end <- d$breaks[2:bLength] 
  
  bprint(start)
  bprint(end)
  bprint(counts)

  
  
  # contains c(x_min, x_max, y_min, y_max)
  if(horizontal)
    ranges <- c(make_data_ranges(c(0,counts)), make_data_ranges(d$breaks))
  else
    ranges <- c(make_data_ranges(d$breaks), make_data_ranges( c(0,counts)))
  bprint(ranges)


  if(horizontal)
  {
    ylab = name
    xlab = "count"
  }
  else
  {
    ylab = "count"
    xlab = name
  }

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
  if(horizontal)
    plot1$add_layer(hbar(bottom = start, top = end, width = counts, ...))
  else
    plot1$add_layer(vbar(left = start, right = end, height = counts, ...))


  draw_x_axes(plot1, ranges, xlab)
  draw_y_axes(plot1, ranges, ylab) 

  if(!is.null(title))
    add_title(plot1, ranges, title)

  plot1
}


make_dodge_start <- function(startVect, n)
{
  relPos <- seq(from = .1, to = 0.9, length.out = n+1)[1:n]
  
  finalVect <- rep(0, length(relPos) * n)
  
  pos <- 1
  for(i in startVect)
    for(j in relPos)
    {
      finalVect[pos] <- i + j
      pos <- pos + 1
    }

  finalVect
}

make_dodge_end <- function(endVect, n)
{
  relPos <- seq(from = .9, to = 0.1, length.out = n+1)[n:1]
  
  finalVect <- rep(0, length(relPos) * n)
  
  pos <- 1
  for(i in endVect)
    for(j in relPos)
    {
      finalVect[pos] <- i + j
      pos <- pos + 1
    }

  finalVect
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

qtbar <- function(data, splitBy = rep(1,length(data)), horizontal = FALSE, position = "stack", color = rainbow(length(unique(splitBy))), title=NULL, name = names(data))
{
  
  counts <- table(data,splitBy)
  print(counts)
  bottoms <- array(0, dim(counts))
  labelNames <- dimnames(counts)[[1]]
  colorNames <- dimnames(counts)[[2]]

  bLength <- nrow(counts)
  
  if (position == "dodge")
  {
    start <- make_dodge_start( 0:(bLength-1), length(colorNames) )
    end <-   make_dodge_end( 0:(bLength-1), length(colorNames) )
    counts <- apply(counts, 1, rbind)
   #counts <- counts
    #color <- color
  }
  else
  {
    #(position = "stack")
    
    start <- 0:(bLength-1)+.1
    end <- 1:bLength-.1
    
    # make the counts be stacked (cumulative)
    for(i in 1:nrow(counts))
      counts[i,] <- cumsum(counts[i,])
    
    #make the bottoms "stack"
    if(ncol(bottoms) > 1)
      bottoms[,2:ncol(bottoms)] <- counts[,1:(ncol(counts) - 1)]
      
    bottoms[,1] <- 0
    color <- rep(color, each = length(labelNames))

  }
  
  #make counts a vector
  counts <- c(counts, NULL)
  
  bprint(start)
  bprint(end)
  bprint(counts)

  # contains c(x_min, x_max, y_min, y_max)
  if(horizontal)
    ranges <- c( make_data_ranges(c(0,counts * 1.1)), make_data_ranges(0:bLength))
  else
    ranges <- c( make_data_ranges(0:bLength), make_data_ranges(c(0,counts * 1.1)))

  bprint(ranges)
  
  if(horizontal)
  {
    ylab = name
    xlab = "count"
  }
  else
  {
    ylab = "count"
    xlab = name
  }


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
  if(length(colorNames) == 1)
    color <- "grey20"
    
  if(horizontal)
    plot1$add_layer(hbar(bottom = start, top = end, width = counts,left = bottoms, fill=color))
  else
    plot1$add_layer(vbar(left = start, right = end, height = counts, bottom = bottoms, fill=color))

  ## add axes
  if(horizontal)
  {
    draw_x_axes(plot1, ranges, xlab)
    draw_y_axes_with_labels(plot1, ranges, labelNames, end - 0.5, ylab)
  }
  else
  {
    draw_x_axes_with_labels(plot1, ranges, labelNames, end - 0.5, xlab)
    draw_y_axes(plot1, ranges, ylab)    
  }
  
  if(!is.null(title))
    add_title(plot1, ranges, title)

  plot1
}



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

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
