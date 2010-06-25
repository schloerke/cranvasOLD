make_pretty_axes <- function(dataRange, minimum, maximum)
{
  prettyness <- pretty(dataRange)
  prettyness <- prettyness[prettyness >= minimum]
  prettyness <- prettyness[prettyness <= maximum]
  
  bprint(prettyness)
  prettyness
}

#' draw grid
#' draws the grid from the numeric data ranges
#'
#' @param plotObj Qt plot object to have the layer added to
#' @param dataRanges ranges of the data so a buffer of space may be added
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  dataRanges <- c(0,1,2,3)
#'  draw_grid(make_new_plot(make_window_ranges(dataRanges)), dataRanges)
draw_grid <- function(plotObj, dataRanges, row=0L,col=0L)
{

  xGridLines <- make_pretty_axes(dataRanges[1:2], dataRanges[1], dataRanges[2])
  
  yGridLines <- make_pretty_axes(dataRanges[3:4], dataRanges[3], dataRanges[4])
  
  bprint(xGridLines)
  bprint(yGridLines)
  draw_grid_with_positions(plotObj, dataRanges, xGridLines, yGridLines, row=row, col=col)

}

#' draw grid
#' draws the grid at given positions
#'
#' @param plotObj Qt plot object to have the layer added to
#' @param dataRanges ranges of the data so a buffer of space may be added
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
draw_grid_with_positions <- function(plotObj, dataRanges, horiPos=NULL, vertPos=NULL, minor.horiPos=NULL, minor.vertPos=NULL, row,col)
{

  #background  
  plotObj$add_layer(
    rect(
      left    = dataRanges[1], 
      right   = dataRanges[2],
      bottom  = dataRanges[3], 
      top     = dataRanges[4],
      fill    = "grey80",
      stroke  = "grey80"
    ),row=row,col=col
  )

  #horizontal
  if(!is.null(vertPos))
  {
    plotObj$add_layer(
      line(
        left    = rep(c(dataRanges[1:2],NA), length(vertPos)),
        bottom  = rep(vertPos,each=3),
        stroke  = "white"
      ), row=row,col=col
    )

	minor.vertPos <- vertPos[-length(vertPos)] + diff(vertPos)/2
  }

  #vertical
  if(!is.null(horiPos))
  {
 
    plotObj$add_layer(
      line(
        left    = rep(horiPos,each=3),
        bottom  = rep(c(dataRanges[3:4],NA), length(horiPos)),
        stroke  = "white"
      ),row=row,col=col
    )

	minor.horiPos <- horiPos[-length(horiPos)] + diff(horiPos)/2
  }

  #minor horizontal
  if(!is.null(minor.vertPos))
  {	
    # change linewidth to smaller width
    plotObj$add_layer(
      line(
        left    = rep(c(dataRanges[1:2],NA), length(minor.vertPos)),
        bottom  = rep(minor.vertPos,each=3),
        stroke  = "white",
        width   = 0.1
      ),row=row, col=col
    )

  }

  #minor vertical
  if(!is.null(minor.horiPos))
  { 
  
    # change linewidth to smaller width
    plotObj$add_layer(
      line(
        left    = rep(minor.horiPos,each=3),
        bottom  = rep(c(dataRanges[3:4],NA), length(minor.horiPos)),
        stroke  = "white",
        width   = 0.1
      ), row=row,col=col
    )
  }



}

#' draw x axes
#' draws the x axes from the numeric data range
#'
#' @param plotObj Qt plot object to have the layer added to
#' @param dataRanges ranges of the data so a buffer of space may be added
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  dataRanges <- c(0,1,2,3)
#'  draw_x_axes(make_new_plot(make_window_ranges(dataRanges)), dataRanges)
draw_x_axes <- function(plotObj, dataRanges, name)
{
  xRangeLabels <- make_pretty_axes(dataRanges[1:2], dataRanges[1], dataRanges[2])
  
  draw_x_axes_with_labels(plotObj, dataRanges, xRangeLabels, xRangeLabels, name)

}

#' draw x axes
#' draws the x axes with the labels and label positions given
#'
#' @param plotObj Qt plot object to have the layer added to
#' @param dataRanges ranges of the data so a buffer of space may be added
#' @param axisLabels vector of labels 
#' @param labelHoriPos horizontal position of the axisLabels
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
draw_x_axes_with_labels <- function(plotObj, dataRanges, axisLabels, labelHoriPos, name = NULL)
{
  #  X label
  x_left <- range(dataRanges[1:2])
  x_bottom <- c(dataRanges[3],dataRanges[3])
  x_bottom <- x_bottom - 0.03 * diff(dataRanges[3:4])
  x_labelpos <- dataRanges[3] - 0.03 * diff(dataRanges[3:4])

#  plotObj$add_layer(line(left=x_left,bottom=x_bottom,stroke="grey"))
# draw tick marks
  plotObj$add_layer(line(
  	left=rep(labelHoriPos,each=3),
  	bottom=rep(c(dataRanges[3],dataRanges[3] - 0.02 * diff(dataRanges[3:4]),NA), length(labelHoriPos)),
  	stroke="grey30"))
  
  plotObj$add_layer(
    text(
      text=axisLabels,
      left=labelHoriPos,
      bottom=x_labelpos, 
      stroke="grey30",
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

#' draw y axes
#' draws the y axes from the numeric data range
#'
#' @param plotObj Qt plot object to have the layer added to
#' @param dataRanges ranges of the data so a buffer of space may be added
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  dataRanges <- c(0,1,2,3)
#'  draw_y_axes(make_new_plot(make_window_ranges(dataRanges)), dataRanges)
draw_y_axes <- function(plotObj, dataRanges, name = NULL)
{
  yRangeLabels <- pretty(dataRanges[3:4])
  yRangeLabels <- make_pretty_axes(dataRanges[3:4], dataRanges[3], dataRanges[4])

#  yRangeLabels <- yRangeLabels[yRangeLabels > x_labelpos]
#  yRangeLabels <- c(0, yRangeLabels[yRangeLabels > dataRanges[3] & yRangeLabels <= dataRanges[4]])
  
  draw_y_axes_with_labels(plotObj, dataRanges, as.character(yRangeLabels), yRangeLabels, name)
}

#' draw y axes
#' draws the y axes with the labels and label positions given
#'
#' @param plotObj Qt plot object to have the layer added to
#' @param dataRanges ranges of the data so a buffer of space may be added
#' @param axisLabels vector of labels 
#' @param labelVertPos vertical position of the axisLabels
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
draw_y_axes_with_labels <- function(plotObj, dataRanges, axisLabels, labelVertPos, name = NULL)
{
  #  Y label
  y_left <- dataRanges[1] - 0.03 * diff(dataRanges[1:2])
  y_bottom = dataRanges[3:4]
#  print(y_bottom)
#  y_bottom <- range(y_bottom[y_bottom >= 0 && y_bottom < windowRanges[4]])
  y_labelpos = dataRanges[1] - 0.04 * diff(dataRanges[1:2])


  #draw x and y axes!
#  plotObj$add_layer(line(left=y_left,bottom=y_bottom,stroke="grey"))
  plotObj$add_layer(line(
  	left=rep(c(dataRanges[1] - 0.02 * diff(dataRanges[1:2]), dataRanges[1],NA), length(labelVertPos)),
  	bottom=rep(labelVertPos,each=3),
  	stroke="grey30"))
  
  
  plotObj$add_layer(
    text(
      text = axisLabels, 
      left = y_labelpos, 
      bottom = labelVertPos, 
      stroke = "grey30",
      halign = "right"
    )
  )
  
  if(!is.null(name))
    plotObj$add_layer(
      text(
        text = name,
        left = dataRanges[1] - 0.18 * diff(dataRanges[1:2]),
        bottom = y_bottom[1] + 0.5 * diff(y_bottom),
        stroke = "black",
        valign = "center",
        rot=90
      )
    )  


  bprint(y_left)
  bprint(y_bottom)
  bprint(y_labelpos)
  y_axisLabels <- axisLabels
  bprint(y_axisLabels)
  bprint(labelVertPos)

}
