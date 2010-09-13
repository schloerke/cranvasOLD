source("../utilities/api-sketch.r")
source("../utilities/axes.r")
source("../utilities/helper.r")
source("bprint.r")
library(reshape)
library(plyr)

#' Fill and Stroke by Color
#' Set the fill and stroke by the color if they are already not defined
#'
#' @param color color to be used for (possibly) both the fill and stroke
#' @param fill fill to be used
#' @param color stroke to be used
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @examples
#'   fill_and_stroke(color = "red")
#'   fill_and_stroke(fill = "red", stroke = "black")
#'   fill_and_stroke(color = "red", stroke = "black")
#'   fill_and_stroke(color = "red", fill = "black")
fill_and_stroke <- function(color=NULL, fill=NULL, stroke=NULL) {  
  if (is.null(stroke)) stroke = color  
  if (is.null(fill)) fill = color
  list(fill=fill, stroke = stroke)
}

divide_by_maximum <- function(val, maxVal= val) {
  maxValue <- max(maxVal)
  if(maxValue != 0)
    val / maxValue
  else 
    val
}

zero_then_top_by_order <- function(top, top_order) {
  top_order <- order(top)
  
  c(0, top[top_order[-length(top_order)]])
}


#' continuous_to_bars(mtcars$disp, mtcars$cyl, position = "dodge", stroke = "black")
#' continuous_to_bars(mtcars$disp, mtcars$cyl, position = "identity", stroke = "black")
#' continuous_to_bars(mtcars$disp, mtcars$cyl, position = "relative", stroke = "black")
#' continuous_to_bars(mtcars$disp, mtcars$cyl, position = "stack", stroke = "black")
continuous_to_bars <- function(data = NULL, splitBy = NULL, position = "none", color = NULL, fill = NULL, stroke = NULL, ...) {
  
  original = list(
      data = data, 
      splitBy = splitBy,
      color = color,
      stroke = stroke,
      fill = fill,
      position = position
    ) 
  
  breaks <- suppressWarnings(hist(data,plot=FALSE,...))$breaks
  break_len <- length(breaks)

  bar_top <- table(cut(data, breaks = breaks), splitBy)  
  
  data_pos <- melt(bar_top)
  names(data_pos) <- c("label", "group", "top")

  label_names <- unique(data_pos$label)
  group_names <- unique(data_pos$group)

  data_pos$bottom <- rep(0, nrow(data_pos))

  if(is.null(color)) {
    if(length(group_names) == 1) {
      data_pos$color <- rep("grey20", nrow(data_pos))      
    } else {    
      data_pos$color <- rep(rainbow(length(group_names)), each = length(label_names))
    }
  }
    
  if (position == "dodge") {
    
    pos <- make_dodge_pos( breaks, length(group_names))
    data_pos$left <- pos$start
    data_pos$right <- pos$end
  } else  {
    # (position == "stack" || position == "relative")

    data_pos$left <- rep(breaks[1:(break_len-1)], length(group_names))
    data_pos$right <- rep(breaks[2:break_len] , length(group_names))
    
    if(position != "identity") {
      # make the bar_top be stacked (cumulative)
      for (i in 1:nrow(bar_top)) {
        bar_top[i,] <- cumsum(bar_top[i,])
      }
      data_pos <- ddply(data_pos, c("label"), transform, top = cumsum(top))
    }
    
    #make the bar_bottom "stack"
    data_pos <- data_pos[order(data_pos$top),]
    data_pos <- ddply(data_pos, "label", transform, bottom = zero_then_top_by_order(top, order))

    # relative      
    if (position == "relative") {
      data_pos <- ddply(data_pos, c("label"), transform, bottom = divide_by_maximum(bottom, top))
      data_pos <- ddply(data_pos, c("label"), transform, top = divide_by_maximum(top))
    }
  }
  
  # Color Management
  f_and_s <- fill_and_stroke(data_pos$color, fill = fill, stroke = stroke)
  data_pos$fill = f_and_s$fill
  data_pos$stroke = f_and_s$stroke
  data_pos$color = NULL

  list(
    data = data_pos,
    breaks = breaks,
    label_names = label_names,
    group_names = group_names,
    original = original
  )

}

#' Create a dot plot
#' Create a dot plot from 1-D numeric data
#'
#' http://content.answcdn.com/main/content/img/oxford/Oxford_Statistics/0199541454.dot-plot.1.jpg
#'
#' @param data vector of numeric data to be made into a histogram
#' @param horizontal boolean to decide if the bars are horizontal or vertical
#' @param ... arguments supplied to hist() or the hist layer
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  # toture
#'    qtdot(rnorm(1000000), floor(rnorm(1000000)*3))
#'    qtdot(rnorm(1000000), floor(runif(1000000)*15), title = "Toture - stack") # each column is split evenly
#'    qtdot(rnorm(1000000), floor(runif(1000000)*15), title = "Toture - dodge", position = "dodge") # each column has similar height colors
#'    qtdot(rnorm(1000000), floor(runif(1000000)*15), title = "Toture - relative", position = "relative") # range from 0 to 1
#'  # color tests
#'    qtdot(mtcars$disp, horizontal = TRUE, fill = "gold", stroke = "red4")
#'    qtdot(mtcars$disp, mtcars$cyl, stroke = "black", position = "stack")
#'    qtdot(mtcars$disp, mtcars$cyl, stroke = "black", position = "identity")
#'    qtdot(mtcars$disp, mtcars$cyl, stroke = "black", position = "dodge")
#'    qtdot(mtcars$disp, mtcars$cyl, stroke = "black", position = "relative")
qtdot <- function(
  data, 
  splitBy = rep(1, length(data)), 
  horizontal = TRUE,
  position = "none",
  color = NULL,
  fill = NULL,
  stroke = NULL,
  title = NULL,
  name = names(data),
  ...
) {

  bars_info <- continuous_to_bars(data, splitBy, position, color, fill, stroke, ...)
  bars <- bars_info$data
  color <- bars$color  

    
  # contains c(x_min, x_max, y_min, y_max)
  if (horizontal) {
    ranges <- c(make_data_ranges(c(0, bars$top)), make_data_ranges(bars_info$breaks))
  } else {
    ranges <- c(make_data_ranges(bars_info$breaks), make_data_ranges( c(0, bars$top)))
  }
  
  if (horizontal) {
    ylab = name
    xlab = "count"
  } else {
    ylab = "count"
    xlab = name
  }

  # create the plot
  # window size 600 x 600; xrange and yrange from above
  windowRanges <- make_window_ranges(ranges, xlab, ylab)
  plot1<-make_new_plot(windowRanges)

  # draw grid
  if (horizontal) {
    draw_grid_with_positions(plot1, ranges, make_pretty_axes(ranges[1:2], ranges[1], ranges[2]), NULL)
  } else {
    draw_grid_with_positions(plot1, ranges, NULL, make_pretty_axes(ranges[3:4], ranges[3], ranges[4]))
  }
    
  
  # for different representations of the data (shape, color, etc) pass vecor arguments for shape, color, x, y
  # c(obj) makes a matrix into a vector
  if (horizontal) {
    plot1$add_layer(hbar(bottom = c(bars$left), top = c(bars$right), width = c(bars$top), left = c(bars$bottom), fill = c(bars$fill), stroke = c(bars$stroke)))
  } else {
    plot1$add_layer(vbar(left = c(bars$left), right = c(bars$right), height = c(bars$top), bottom = c(bars$bottom), fill = c(bars$fill), stroke = c(bars$stroke)))
  }

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
  
  bars_info <- continuous_to_bars(data, splitBy, position, color, fill, stroke, ...)
  bars <- bars_info$data
  color <- bars$color  

    
  # contains c(x_min, x_max, y_min, y_max)
  if (horizontal) {
    ranges <- c(make_data_ranges(c(0, bars$top)), make_data_ranges(bars_info$breaks))
  } else {
    ranges <- c(make_data_ranges(bars_info$breaks), make_data_ranges( c(0, bars$top)))
  }

  if (horizontal) {
    ylab = name
    xlab = "count"
  } else {
    ylab = "count"
    xlab = name
  }

  # create the plot
  # window size 600 x 600; xrange and yrange from above
  windowRanges <- make_window_ranges(ranges, xlab, ylab)
  plot1<-make_new_plot(windowRanges)

  # draw grid
  if (horizontal) {
    draw_grid_with_positions(plot1, ranges, make_pretty_axes(ranges[1:2], ranges[1], ranges[2]), NULL)
  } else {
    draw_grid_with_positions(plot1, ranges, NULL, make_pretty_axes(ranges[3:4], ranges[3], ranges[4]))
  }
    
  
  # for different representations of the data (shape, color, etc) pass vecor arguments for shape, color, x, y
  # c(obj) makes a matrix into a vector
  if (horizontal) {
    plot1$add_layer(hbar(bottom = c(bars$left), top = c(bars$right), width = c(bars$top), left = c(bars$bottom), fill = c(bars$fill), stroke = c(bars$stroke)))
  } else {
    plot1$add_layer(vbar(left = c(bars$left), right = c(bars$right), height = c(bars$top), bottom = c(bars$bottom), fill = c(bars$fill), stroke = c(bars$stroke)))
  }

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
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @examples
#'  make_dodge_pos(c(1:5), 3)
make_dodge_pos <- function(breaks, n) {
  gap <- diff(breaks[1:2])
  breaks <- breaks[-length(breaks)]
  
  relPos <- seq(from = gap*.1, to = gap * .9, length.out = n+1)
  startRel <- relPos[-(n+1)]
  endRel <- relPos[-1]
  
  starts <- c(sapply(startRel, function(x) { 
    x + breaks
  }))
  ends <- c(sapply(endRel, function(x) { 
    x + breaks
  }))

  data.frame(start = starts, end = ends)  
}


