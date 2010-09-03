source("../utilities/api-sketch.r")
source("../utilities/axes.r")
source("../utilities/helper.r")
source("bprint.r")
library(reshape)
library(plyr)

divide_by_maximum <- function(x) {
  x / max(x)
}

zero_then_top_by_order <- function(top, top_order) {
  top_order <- order(top)
  
  c(0, top[top_order[-length(top_order)]])
}

continuous_to_bars <- function(data, splitBy, position, color, fill, stroke, ...) {
  d <- suppressWarnings(hist(data,plot=FALSE,...))
  breaks <- d$breaks
#  bprint(breaks)
  break_len <- length(breaks)
  bar_top <- table(cut(data, breaks = breaks), splitBy)  
#  bprint(bar_top)
  
  data_pos <- melt(bar_top)
  names(data_pos) <- c("label", "group", "top")
  print(data_pos)
  data_pos$bottom <- rep(0, nrow(data_pos))
  
  bar_bottom <- array(0, dim(bar_top))
  label_names <- dimnames(bar_top)[[1]]
  split_names <- dimnames(bar_top)[[2]]
#  bprint(label_names)
#  bprint(split_names)
    
  if (position == "dodge") {
    pos <- make_dodge_pos( breaks, length(split_names))
    bar_left  <- pos$start
    data_pos$left <- pos$start
    bar_right <- pos$end
    data_pos$right <- pos$start
    bar_top <- apply(bar_top, 1, rbind)
        
    color <- rep(color, length(label_names))
    data_pos$color <- rep(color, length(label_names))
  } else  {
    # (position == "stack" || position == "relative")
    
    
    color <- rep(color, each <- length(label_names))
    data_pos$color <- rep(color, each = length(split_names))

    #(position = "stack")
    bar_left <- rep(breaks[1:(break_len-1)], length(split_names))
    bar_right <- rep(breaks[2:break_len] , length(split_names))
    data_pos$left <- rep(breaks[1:(break_len-1)], length(split_names))
    data_pos$right <- rep(breaks[2:break_len] , length(split_names))
    
    
    if(position != "identity") {
      # make the bar_top be stacked (cumulative)
      for (i in 1:nrow(bar_top)) {
        bar_top[i,] <- cumsum(bar_top[i,])
      }
      data_pos <- ddply(data_pos, c("label"), transform, top = cumsum(top))
    }
    
    #make the bar_bottom "stack"
    if (ncol(bar_bottom) > 1) {
      bar_bottom[,2:ncol(bar_bottom)] <- bar_top[,1:(ncol(bar_top) - 1)]
    }
#    data_pos$bottom[-1] <- data_pos$top[-nrow(data_pos)]
    data_pos <- data_pos[order(data_pos$top),]
    data_pos <- ddply(data_pos, "label", transform, bottom = zero_then_top_by_order(top, order))

    bar_bottom[,1] <- 0
    
    
    # spine-o-gram      
    if (position == "relative") {
      for (i in 1:nrow(bar_bottom)) {
        bar_bottom[i,] <- bar_bottom[i,] / max(bar_top[i,])
      }
      data_pos <- ddply(data_pos, c("label"), transform, bottom = divide_by_maximum(bottom))

      for (i in 1:nrow(bar_top)) {
        bar_top[i,] <- bar_top[i,] / max(bar_top[i,])
      }
      data_pos <- ddply(data_pos, c("label"), transform, top = divide_by_maximum(top))
    }
  }
  bar_top <- c(bar_top)
  bar_bottom <- c(bar_bottom)
  
  if (is.null(color)) {
    if (length(unique(data_pos$group)) == 1) {
      data_pos$color <- rep("grey20", nrow(data_pos))
    } else {
      if(position == "dodge") {
        color <- rep(rainbow(length(split_names)), length(label_names))        
      } else {
        color <- rep(rainbow(length(split_names)), each = length(label_names))
      }
    }
  }
  
  if (is.null(stroke)) {
    stroke = color
  }
  if (is.null(fill)) {
    fill = color
  }

  
  print(data_pos)

  list(
    data_pos = data_pos,
    top    = bar_top, 
    bottom = bar_bottom, 
    left   = bar_left, 
    right  = bar_right, 
    label_names = label_names, 
    group_names = split_names,
    color = color, 
    breaks = breaks
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
#'    qthist(rnorm(1000000), floor(rnorm(1000000)*3))
#'    qthist(rnorm(1000000), floor(runif(1000000)*15), title = "Toture - stack") # each column is split evenly
#'    qthist(rnorm(1000000), floor(runif(1000000)*15), title = "Toture - dodge", position = "dodge") # each column has similar height colors
#'    qthist(rnorm(1000000), floor(runif(1000000)*15), title = "Toture - relative", position = "relative") # range from 0 to 1
#'  # color tests
#'    qthist(mtcars$disp, horizontal = TRUE, fill = "gold", stroke = "red4")
#'    qthist(mtcars$disp, mtcars$cyl, stroke = "black")
#'    qthist(mtcars$disp, mtcars$cyl, position = "dodge", stroke = "black")
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

  bars <- continuous_to_bars(data, splitBy, position, color, fill, stroke, ...)
#  bprint(bars)
  color <- bars$color  

#  bprint(bars$left)
#  bprint(bars$right)
#  bprint(bars$top)
#  bprint(bars$bottom)
#  bprint(bars$color)

    
  # contains c(x_min, x_max, y_min, y_max)
  if (horizontal) {
    ranges <- c(make_data_ranges(c(0, bars$top)), make_data_ranges(bars$breaks))
  } else {
    ranges <- c(make_data_ranges(bars$breaks), make_data_ranges( c(0, bars$top)))
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
#    plot1$add_layer(hbar(bottom = bars$left, top = bars$right, width = bars$top, ...))
#  else
#    plot1$add_layer(vbar(left = bars$left, right = bars$right, height = bars$top, ...))

  # c(obj) makes a matrix into a vector
  if(horizontal)
    plot1$add_layer(hbar(bottom = c(bars$left), top = c(bars$right), width = c(bars$top), left = c(bars$bottom), fill=fill, stroke = stroke))
  else
    plot1$add_layer(vbar(left = c(bars$left), right = c(bars$right), height = c(bars$top), bottom = c(bars$bottom), fill=fill, stroke = stroke))

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
  
  bars <- continuous_to_bars(data, splitBy, position, color, fill, stroke, ...)
  color <- bars$color  

#  bprint(bars$left)
#  bprint(bars$right)
#  bprint(bars$top)
#  bprint(bars$bottom)
#  bprint(bars$color)

  if (is.null(color)) {
    if (length(bars$group_names) == 1) {
      color <- "grey20"
    } else {
      if(position == "dodge") {
        color <- rep(rainbow(length(unique(splitBy))), length(bars$label_names))        
      } else {
        color <- rep(rainbow(length(unique(splitBy))), each = length(bars$label_names))
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
    ranges <- c(make_data_ranges(c(0, bars$top)), make_data_ranges(bars$breaks))
  } else {
    ranges <- c(make_data_ranges(bars$breaks), make_data_ranges( c(0, bars$top)))
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
#    plot1$add_layer(hbar(bottom = bars$left, top = bars$right, width = bars$top, ...))
#  else
#    plot1$add_layer(vbar(left = bars$left, right = bars$right, height = bars$top, ...))

  # c(obj) makes a matrix into a vector
  if(horizontal)
    plot1$add_layer(hbar(bottom = c(bars$left), top = c(bars$right), width = c(bars$top), left = c(bars$bottom), fill=fill, stroke = stroke))
  else
    plot1$add_layer(vbar(left = c(bars$left), right = c(bars$right), height = c(bars$top), bottom = c(bars$bottom), fill=fill, stroke = stroke))

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


